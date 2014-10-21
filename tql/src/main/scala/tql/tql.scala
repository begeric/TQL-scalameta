package tql

/**
 * Created by Eric on 20.10.2014.
 */

trait Traverser[T] {

  import scala.reflect.{ClassTag, classTag}

  type MatcherResult[A] = Option[(T, A)]

  abstract class TreeMapper[+A] extends (T => MatcherResult[A]) {

    def andThen[B : Monoid](m: =>  TreeMapper[B]) = TreeMapper[B] { tree =>
      this(tree) match {
        case Some((v, t)) => m(v)
        case _ => None
      }
    }

    def ~[B : Monoid](m: =>  TreeMapper[B]) = andThen(m)

    def compose[B >: A](m: => TreeMapper[B])(implicit x: Monoid[B]) = TreeMapper[B] { tree =>
      this(tree) match {
        case Some((a1, a2)) => m(a1) match {
          case Some((b1, b2)) => Some((b1, x.append(a2, b2)))
          case t => t
        }
        case _ => m(tree)
      }
    }

    def +[B >: A](m: => TreeMapper[B])(implicit x: Monoid[B]) = compose(m)(x)

    def map[B](f: A => B) = TreeMapper[B] { tree =>
      for {
        (v, t) <- this(tree)
      } yield ((v, f(t)))
    }

    def orElse[B >: A : Monoid](m: => TreeMapper[B]) = TreeMapper[B] { tree  =>
      this(tree) match {
        case None => m(tree)
        case  s => s
      }
    }

    def |[B >: A : Monoid](m: => TreeMapper[B]) = orElse(m)
  }

  def TreeMapper[A](f: T => MatcherResult[A]): TreeMapper[A] = new TreeMapper[A] {
    override def apply(tree: T): MatcherResult[A] = f(tree)
  }

  def traverse[A : Monoid](tree: T, f: TreeMapper[A]): MatcherResult[A]

}