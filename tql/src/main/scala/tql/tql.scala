package tql

/**
 * Created by Eric on 20.10.2014.
 */

trait Traverser[T] {

  import scala.reflect.{ClassTag, classTag}

  type MatcherResult[U, A] = Option[(U, A)]

  abstract class TreeMapper[-U <: T : ClassTag, +V <: T, +A] extends (T => MatcherResult[V, A]) {

    def andThen[Z <: T, B : Monoid](m: =>  TreeMapper[V, Z, B]) = TreeMapper[U, Z, B] { tree =>
      this(tree) match {
        case Some((v, t)) => m(v)
        case _ => None
      }
    }

    def ~[Z <: T, B : Monoid](m: =>  TreeMapper[V, Z, B]) = andThen(m)

    def compose[Z <: T, B >: A](m: => TreeMapper[V, Z, B])(implicit x: Monoid[B]) = TreeMapper[U, Z, B] { tree =>
      this(tree) match {
        case Some((a1, a2)) => m(a1) match {
          case Some((b1, b2)) => Some((b1, x.append(a2, b2)))
          case t => t
        }
        case _ => m(tree)
      }
    }

    def +[Z <: T, B >: A](m: => TreeMapper[V, Z, B])(implicit x: Monoid[B]) = compose(m)(x)

    def map[B](f: A => B) = TreeMapper[U, V, B] { tree =>
      for {
        (v, t) <- this(tree)
      } yield ((v, f(t)))
    }

    def orElse[X <: U : ClassTag, Y >: V <: T, B >: A : Monoid](m: => TreeMapper[X, Y, B]) = TreeMapper[X, Y, B] { tree: X =>
      this(tree) match {
        case None => m(tree)
        case  s => s
      }
    }

    def |[X <: U : ClassTag, Y >: V <: T, B >: A : Monoid](m: => TreeMapper[X, Y, B]) = orElse(m)
  }

  def TreeMapper[U <: T : ClassTag, V <: T, A](f: U => MatcherResult[V, A]): TreeMapper[U, V, A] = new TreeMapper[U, V, A] {
    override def apply(tree: T): MatcherResult[V, A] = tree match {
      case t: U => f(t)
      case _ => None
    }
  }

  def traverse[U <: T : ClassTag, V <: T, A : Monoid](tree: U, f: TreeMapper[U, V, A]): MatcherResult[U, A]

}