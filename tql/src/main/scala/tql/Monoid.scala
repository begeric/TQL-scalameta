package tql

/**
 * Created by Eric on 20.10.2014.
 */

/**
 * https://en.wikipedia.org/wiki/Monoid
 * */
trait Monoid[A]{
  def zero: A
  def append(a: A, b: A): A
}

/*Allows to write a + b rather than
* implicitly[Monoid[A]].append(a, b)
* */
object MonoidEnhencer {
  implicit class MonoidEnhencer[A](a: A)(implicit m: Monoid[A]){
    def + (b: A) = m.append(a, b)
  }
}
