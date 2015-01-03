package tql

/**
 * Created by Eric on 19.10.2014.
 */

import scala.language.implicitConversions
import scala.reflect.ClassTag

trait SyntaxEnhancer[T] { self: Combinators[T] with Traverser[T] =>

  implicit class TEnhancer(t: T){

    def >>[A](a: Matcher[A]) = a(t)

    def resultOf[A : Monoid](a: Matcher[A]) = new MatcherResultEnhancer(a(t)).result
    def treeOf[A : Monoid](a: Matcher[A]) = new MatcherResultEnhancer(a(t)).tree
  }

  /*Required for things inside TreeMapperEnhancer*/
  def downBreakAlias[A : Monoid](m: Matcher[A]) = downBreak(m)
  def downAlias[A : Monoid](m: Matcher[A]) = down(m)
  def upBreakAlias[A : Monoid](m: Matcher[A]) = upBreak(m)
  def upAlias[A : Monoid](m: Matcher[A]) = up(m)
  def childrenAlias[A : Monoid](m: Matcher[A]) = children(m)

  implicit class TreeMapperEnhancer[A](a: Matcher[A]){
    //def >>[B] (f: T => MatchResult[B]) = flatMap(f)
    /*def apply[I <: T : ClassTag, O <: T]
             (f: PartialFunction[I, O])
             (implicit x: ClassTag[T], y: AllowedTransformation[I, O]) =
      transformWithResult[I, O](f)  */

    def collect = a.map(List(_))
    def downBreak(implicit x: Monoid[A]) = downBreakAlias(a)
    def down(implicit x: Monoid[A]) = downAlias(a)
    def upBreak(implicit x: Monoid[A]) = upBreakAlias(a)
    def up(implicit x: Monoid[A]) = upAlias(a)
    def children(implicit x: Monoid[A]) = childrenAlias(a)
  }


  /**
   * Convention : Operators ending with : are right assosiative
   * Moreover a \: b is desugared to b.\:(a)
   */
  implicit class MatcherXPath[A : Monoid](a: Matcher[A]){
    def \: (t: T) = downBreakAlias(a).apply(t)
    def \\: (t: T) = downAlias(a).apply(t)
    def \:[B] (b: Matcher[B]) = b andThen downBreakAlias(a)
    def \\:[B] (b: Matcher[B]) = b andThen downAlias(a)
  }

  implicit class MatcherResultEnhancer[A](a: MatchResult[A]){
    def result(implicit x: Monoid[A])  = a.map(_._2).getOrElse(x.zero)
    def tree    = a.map(_._1)
  }

  implicit def MatcherResultToResult[A : Monoid](a: MatchResult[A]): A = a.result
  implicit def MatcherResultToTree(a : MatchResult[_]): Option[T] = a.tree

}
