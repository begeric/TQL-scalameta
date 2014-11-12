package tql

/**
 * Created by Eric on 20.10.2014.
 */

/*Allows to write a + b rather than
* implicitly[Monoid[A]].append(a, b)
* */
object MonoidEnhencer {
  implicit class MonoidEnhencer[A : Monoid](a: A){
    def + (b: A) = implicitly[Monoid[A]].append(a, b)
  }
}
