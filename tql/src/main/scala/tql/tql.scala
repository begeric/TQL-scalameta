package tql

/**
 * Created by Eric on 20.10.2014.
 */

trait Traverser[T] {

  import scala.reflect.{ClassTag, classTag}

  type MatcherResult[A] = Option[(T, A)]

  /**
   * A TreeMapper is a function or 'combinator' which takes a T and return an Option of tuple of
   * - a transformed T (or the same)
   * - a result of type A s.t Exists Monoid[A] so that results can be combined during the traversal
   * A transformation/traversal as succeeded if the result of the application on the TreeMapper is not a None
   * */
  abstract class TreeMapper[+A] extends (T => MatcherResult[A]) {

    /**
     * a andThen b
     * b is 'executed' only if a succeeded
     * We discard the transformation and result of a
     * */
    def andThen[B : Monoid](m: =>  TreeMapper[B]) = TreeMapper[B] { tree =>
      this(tree) match {
        case Some((v, t)) => m(v)
        case _ => None
      }
    }

    /**
     * Alias for andThen
     * */
    def ~[B : Monoid](m: =>  TreeMapper[B]) = andThen(m)

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
        case Some((a1, a2)) => m(a1) match {
          case Some((b1, b2)) => Some((b1, x.append(a2, b2)))
          case t => t
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
  }

  def TreeMapper[A](f: T => MatcherResult[A]): TreeMapper[A] = new TreeMapper[A] {
    override def apply(tree: T): MatcherResult[A] = f(tree)
  }

  def traverse[A : Monoid](tree: T, f: TreeMapper[A]): MatcherResult[A]

}