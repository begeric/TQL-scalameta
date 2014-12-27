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

  /**
   * Sometimes the Scala parser/compiler is not happy about Monoid 'addition' and things like
   * a + b where a and b are monoids of type A won't work because it thinks the + is for a String addition.
   *
   * found   : C
   * required: String
   *
   * (from what I can gather it doesnt work when there is a type parameter C >: B : Monoid and then we want to do
   * 'addition' on two monoids of type C)
   * Even if one defines another operator, say |+|, it stills fails with an error of that kind:
   *  Error:(81, 45) value |+| is not a member of type parameter C
   *
   * Fun fact: intelliji actually parses things correctly!
   *
   * So this is why the weird object M exists, to 'lift' a Monoid.
   * */
  object M {
    def apply[A: Monoid](a: A) = new MonoidEnhencer(a)

    /**
     * Chosed because:
     * 1) left-associative
     * 2) higher precedence than + which is used in MonoidEnhencer
     * 3) the less ugly of all choices
     * */
    def %[A: Monoid](a: A) = new MonoidEnhencer(a)
  }
}
