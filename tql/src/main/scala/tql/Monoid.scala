package tql

/**
 * Created by Eric on 20.10.2014.
 */
trait Monoid[A]{
  def zero: A
  def append(a: A, b: A): A
}

object MonoidEnhencer {
  implicit class MonoidEnhencer[A](a: A)(implicit m: Monoid[A]){
    def + (b: A) = m.append(a, b)
  }
}
