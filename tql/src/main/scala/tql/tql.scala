package tql

/**
 * Created by Eric on 20.10.2014.
 */

trait Traverser[T] {

  type MatcherResult[A] = Option[(T, A)]

  /**
   * A TreeMapper is a function or 'combinator' which takes a T and return an Option of tuple of
   * - a transformed T (or the same)
   * - a result of type A s.t Exists Monoid[A] so that results can be combined during the traversal
   * A transformation/traversal has succeeded if the result of the application on the TreeMapper is not a None
   * */
  abstract class TreeMapper[+A] extends (T => MatcherResult[A]) {

    /**
     * a andThen b
     * b is 'executed' only if a succeeded
     * We discard the result of a
     * */
    def andThen[B : Monoid](m: => TreeMapper[B]) = TreeMapper[B] { tree =>
      this(tree) match {
        case Some((t, v)) => m(t)
        case _ => None
      }
    }

    /**
     * Alias for andThen
     * */
    def ~>[B : Monoid](m: =>  TreeMapper[B]) = andThen(m)

    /**
     * a andThenLeft b
     * b is 'executed' only if a succeeded
     * We discard the result of b and only keep the result of a
     * */
    def andThenLeft[B](m: => TreeMapper[B]) = TreeMapper[A] { tree =>
      this(tree) match {
        case Some((t, v)) => m(t) map (x => (x._1, v))
        case _ => None
      }
    }

    /**
     * Alias for andThenLeft
     * */
    def <~[B : Monoid](m: =>  TreeMapper[B]) = andThenLeft(m)

    /**
     * Combine the result of two TreeMappers in a tuple2.
     * The order is important, as the the second transformation is applied on the
     * result of the first one.
     * */
    def aggregate[B, C >: A](m: => TreeMapper[B])(implicit x: Monoid[C], y: Monoid[B]) = TreeMapper[(C, B)] { tree =>
      this(tree) match {
        case s @ Some((a1, a2)) => m(a1) match {
          case Some((b1, b2)) => Some((b1, (a2, b2)))
          case _ => s map (u => (u._1, (u._2,y.zero)))
        }
        case _ => m(tree) map (u => (u._1, (x.zero, u._2)))
      }
    }
    /**
     * Alias for aggregate
     * */
    def ~[B, C >: A](m: => TreeMapper[B])(implicit x: Monoid[C], y: Monoid[B]) = aggregate[B, C](m)(x, y)

    /**
     * a compose b
     * Apply a and apply b on the result of the application of a. This means:
     *  - b is applied on the transformed T from a
     *  - the results of a and b are composed
     *
     * If a fails then b is applied
     * If b fails then only the result of a is used
     * */
    def compose[B >: A](m: => TreeMapper[B])(implicit x: Monoid[B]) = TreeMapper[B] { tree =>
      this(tree) match {
        case t @ Some((a1, a2)) => m(a1) match {
          case Some((b1, b2)) => Some((b1, x.append(a2, b2)))
          case _ => t
        }
        case _ => m(tree)
      }
    }

    /**
     * Alias for compose
     * */
    def +[B >: A](m: => TreeMapper[B])(implicit x: Monoid[B]) = compose(m)(x)

    /**
     * Transform the result of the TreeMapper, will probably change
     * */
    def map[B](f: A => B) = TreeMapper[B] { tree =>
      for {
        (v, t) <- this(tree)
      } yield ((v, f(t)))
    }

    /**
     * a orElse b
     * Apply b only if a failed
     * */
    def orElse[B >: A : Monoid](m: => TreeMapper[B]) = TreeMapper[B] { tree  =>
      this(tree) match {
        case None => m(tree)
        case  s => s
      }
    }

    /**
     * Alias for orElse
     * */
    def |[B >: A : Monoid](m: => TreeMapper[B]) = orElse(m)


    /**
     * a feed {resa => b}
     * combinator b can use the result (resa) of a.
     * */
    def feed[B : Monoid](m: => A => TreeMapper[B]) = TreeMapper[B] {tree =>
      this(tree) match {
        case Some((t, v)) => m(v)(t)
        case None => None/*what to do ?*/
      }
    }
  }

  def TreeMapper[A](f: T => MatcherResult[A]): TreeMapper[A] = new TreeMapper[A] {
    override def apply(tree: T): MatcherResult[A] = f(tree)
  }

  def traverse[A : Monoid](tree: T, f: TreeMapper[A]): MatcherResult[A]

}