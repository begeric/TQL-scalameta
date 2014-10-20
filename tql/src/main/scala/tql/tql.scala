package tql

/**
 * Created by Eric on 20.10.2014.
 */

trait Traverser[T] {

  type MatcherResult[U, A] = Option[(U, A)]

  abstract class TreeMapper[U <: T, +A] extends (U => MatcherResult[U, A]) {
    def andThen[B](m: =>  TreeMapper[U, B]) = TreeMapper[U, B] {tree: U =>
      this(tree) match {
        case Some((v, t)) => m(v)
        case _ => None
      }
    }

    def +[B >: A](m: => TreeMapper[U, B])(implicit x: Monoid[B]) = TreeMapper[U, B] { tree: U =>
      for {
        (a1, b1) <- this(tree)
        (a2, b2) <- m(a1)
      } yield (a1, x.append(b1, b2))
    }

    def map[B](f: A => B) = TreeMapper[U, B] { tree: U =>
      for {
        (v, t) <- this(tree)
      } yield ((v, f(t)))
    }

    def |[B >: A](m: => TreeMapper[U, B]) = TreeMapper[U, B] { tree: U =>
      this(tree) match {
        case None => m(tree)
        case  s => s
      }
    }
  }

  def TreeMapper[U <: T, A](f: U => MatcherResult[U, A]): TreeMapper[U, A] = new TreeMapper[U, A] {
    override def apply(tree: U): MatcherResult[U, A] = f(tree)
  }

  def traverse[A : Monoid](tree: T, f: TreeMapper[T, A]): MatcherResult[T, A]

}