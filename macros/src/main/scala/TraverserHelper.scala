/**
 * Created by Eric on 21.10.2014.
 */
package tql

import scala.collection.immutable.Seq
import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.reflect._
import scala.reflect.macros.whitebox.Context

/*
* Macro used to create a traverser for type T from case classes
* build(Term.If) is transformed to
* x => x match {
*   case If(cond, thenp, elsep) => for {
*     (a1, a2) <- f(cond)
*     (b1, b2) <- f(thenp)
*     (c1, c2) <- f(elsep)
*   } yield (if ((a1 eq cond) && (b1 eq thenp) && (c1 eq elsep)) x else Term.If(a1, b1, c1), a2 + b2 + c2)
* }
* */

object TraverserHelper {
  def build[T, A](f: Any /*temporary*/, cases: Any*): (T => Option[(T, A)]) = macro TraverserBuilder.buildImpl[T, A]
}

class TraverserBuilder(val c: Context) {
  import c.universe._

  def buildImpl[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree, cases: c.Tree*): c.Tree = {
    val parameter = TermName(c.freshName)

    val casesImpl = cases.map((cas: c.Tree) => {
      val argsList = getParamsWithTypes(cas)
      val parametersList = argsList._1.map(p => pq"$p @ _")
      val extractor = pq"$cas(..${parametersList})"
      cq"$extractor => ${caseMatch[T, A](f, cas, parameter, argsList._1, argsList._2)}"
    })

    q"""
        ($parameter: ${implicitly[c.WeakTypeTag[T]]}) => $parameter match {
          case ..$casesImpl
          case v => Some((v, implicitly[Monoid[${implicitly[c.WeakTypeTag[A]]}]].zero))
        }
    """
  }

  def getParamsWithTypes(obj: c.Tree): (List[TermName], List[c.Type]) = {
    val f = obj.tpe.member(TermName("unapply"))
    val MethodType(_, resultType) = f.typeSignatureIn(obj.tpe)
    val tupleOrNot: c.Type = resultType.typeArgs.head

    val types = if (tupleOrNot.typeConstructor.toString.startsWith("Tuple")) tupleOrNot.typeArgs
                else List(tupleOrNot)   /*This is disgusting, God please give me a better way to do it*/
    val newNames = types.map(_ => TermName(c.freshName))
    (newNames, types)
  }

  def createEnum[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree, name: TermName, typ: c.Type) = {
    val aTpe = implicitly[c.WeakTypeTag[A]]
    val newTree = TermName(c.freshName)
    val newResult = TermName(c.freshName)
    val lhs = pq"($newTree: $typ, $newResult @ _)"
    val rhs = typ match {
      case t if t <:< typeOf[scala.meta.Tree] =>
        Some(q"$f($name)")
      case t if t <:< typeOf[scala.collection.immutable.Seq[scala.meta.Tree]] =>
        Some(q"scalameta.ScalaMetaTraverser.traverseSeq($f, $name)")
      case t if t <:< typeOf[scala.collection.immutable.Seq[scala.collection.immutable.Seq[scala.meta.Tree]]] =>
        Some(q"scalameta.ScalaMetaTraverser.traverseSeqofSeq($f, $name)")
      case t if t <:< typeOf[scala.Option[scala.meta.Tree]] =>
        Some(q"scalameta.ScalaMetaTraverser.optional($f, $name)")
      case _ => None
    }
    rhs.map(r => (newTree, newResult, fq"$lhs <- $r"))
  }


  def caseMatch[T : c.WeakTypeTag, A : c.WeakTypeTag](func: c.Tree, first: c.Tree, origin: TermName,
                                                      names: List[TermName], types: List[c.Type]): c.Tree = {

    val parameters = names.zip(types)
    val enums = parameters.map(x => createEnum[T, A](func, x._1, x._2))
    val forEnums: List[c.Tree] = enums.flatMap(_.map(_._3))
    val results: List[TermName] = enums.flatMap(_.map(_._2))
    if (results.size > 0){
      val addResults = results.tail.foldLeft[c.Tree](q"${results.head}")((a, b) => q"$a + $b")
      val paramsWithNewParams = parameters.unzip._1.zip(enums.map(_.map(_._1)))
      val reconstructParams = paramsWithNewParams.map(x => x._2.getOrElse(x._1))
      val reconstruct = q"$first(..$reconstructParams)"
      val eqList = paramsWithNewParams.foldLeft[c.Tree](q"true")((acc, x) => q"$acc && ${x._2.map(y => q"($y eq ${x._1})").get}")

      val doesReconstruct = q"if ($eqList) $origin else $reconstruct"

        q"""
        for (
          ..$forEnums
        ) yield ($doesReconstruct, $addResults)
      """
    }
    else
      q"Some(($origin, implicitly[Monoid[${implicitly[c.WeakTypeTag[A]]}]].zero))"
  }
}
