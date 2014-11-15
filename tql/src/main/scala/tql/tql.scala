package tql

/**
 * Created by Eric on 20.10.2014.
 */

trait Traverser[T] {

  type MatcherResult[A] = Option[(T, A)]

  /**
   * A Matcher is a function or 'combinator' which takes a T and return an Option of tuple of
   * - a transformed T (or the same)
   * - a result of type A s.t Exists Monoid[A] so that results can be combined during the traversal
   * A transformation/traversal has succeeded if the result of the application on the Matcher is not a None
   * */
  abstract class Matcher[+A] extends (T => MatcherResult[A]) {

    /**
     * a andThen b
     * b is 'executed' only if a succeeded
     * We discard the result of a
     * */
    def andThen[B : Monoid](m: => Matcher[B]) = Matcher[B] { tree =>
      for {
        (t, v) <- this(tree)
        t2 <- m(t)
      } yield t2
    }

    /**
     * Alias for andThen
     * */
    def ~>[B : Monoid](m: =>  Matcher[B]) = andThen(m)

    /**
     * a andThenLeft b
     * b is 'executed' only if a succeeded
     * We discard the result of b and only keep the result of a
     * */
    def andThenLeft[B](m: => Matcher[B]) = Matcher[A] { tree =>
     for {
        (t, v) <- this(tree)
        (t2, v2) <- m(t)
     } yield (t2, v)
    }

    /**
     * Alias for andThenLeft
     * */
    def <~[B : Monoid](m: =>  Matcher[B]) = andThenLeft(m)

    /**
     * Combine the result of two TreeMappers in a tuple2.
     * The order is important, as the the second transformation is applied on the
     * result of the first one.
     * */
    def aggregate[B : Monoid, C >: A : Monoid](m: => Matcher[B]) = Matcher[(C, B)] { tree =>
      this(tree) match {
        case s @ Some((a1, a2)) => m(a1) match {
          case Some((b1, b2)) => Some((b1, (a2, b2)))
          case _ => s map (u => (u._1, (u._2, implicitly[Monoid[B]].zero)))
        }
        case _ => m(tree) map (u => (u._1, (implicitly[Monoid[C]].zero, u._2)))
      }
    }
    /**
     * Alias for aggregate
     * */
    def ~[B : Monoid, C >: A : Monoid](m: => Matcher[B]) = aggregate[B, C](m)

    /**
     * a compose b
     * Apply a and apply b on the result of the application of a. This means:
     *  - b is applied on the transformed T from a
     *  - the results of a and b are composed
     *
     * If a fails then b is applied
     * If b fails then only the result of a is used
     * */
    def compose[B >: A : Monoid](m: => Matcher[B]) = Matcher[B] { tree =>
      this(tree) match {
        case t @ Some((a1, a2)) => m(a1) match {
          case Some((b1, b2)) => Some((b1, implicitly[Monoid[B]].append(a2, b2)))
          case _ => t
        }
        case _ => m(tree)
      }
    }

    /**
     * Alias for compose
     * */
    def +[B >: A : Monoid](m: => Matcher[B]) = compose(m)

    /**
     * a composeResults b
     * Same as 'compose' but discard the transformed trees of a and b
     * b operates on the origin tree, not the one transformed by a
     */
    def composeResults[B >: A : Monoid](m: => Matcher[B]) = Matcher[B] { tree =>
      this(tree) match {
        case t @ Some((_, a2)) => m(tree) match {
          case Some((_, b2)) => Some((tree, implicitly[Monoid[B]].append(a2, b2)))
          case _ => t
        }
        case _ => m(tree)
      }
    }
    
    /**
     * Alias for composeResults
     * */
    def +> [B >: A : Monoid](m: => Matcher[B]) = composeResults[B](m)

    /**
     * Transform the result of the Matcher, will probably change
     * */
    def map[B](f: A => B) = Matcher[B] { tree =>
      for {
        (v, t) <- this(tree)
      } yield ((v, f(t)))
    }

    /**
     * a orElse b
     * Apply b only if a failed
     * */
    def orElse[B >: A : Monoid](m: => Matcher[B]) = Matcher[B] { tree  =>
      this(tree) match {
        case None => m(tree)
        case  s => s
      }
    }

    /**
     * Alias for orElse
     * */
    def |[B >: A : Monoid](m: => Matcher[B]) = orElse(m)


    /**
     * a feed {resa => b}
     * combinator b can use the result (resa) of a.
     * */
    def feed[B : Monoid](m: => A => Matcher[B]) = Matcher[B] {tree =>for {
        (t, v) <- this(tree)
        t2     <- m(v)(t)
      } yield t2
    }
  }

  def Matcher[A](f: T => MatcherResult[A]): Matcher[A] = new Matcher[A] {
    override def apply(tree: T): MatcherResult[A] = f(tree)
  }

  def traverse[A : Monoid](tree: T, f: Matcher[A]): MatcherResult[A]

}