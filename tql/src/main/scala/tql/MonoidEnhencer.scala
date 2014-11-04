package tql

/**
 * Created by Eric on 20.10.2014.
 */

/*Allows to write a + b rather than
* implicitly[Monoid[A]].append(a, b)
* */
object MonoidEnhencer {
  implicit class MonoidEnhencer[A](a: A)(implicit m: Monoid[A]){
    def + (b: A) = m.append(a, b)
  }
}
