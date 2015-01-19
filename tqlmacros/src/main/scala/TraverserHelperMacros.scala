package tql

/**
 * Created by Eric on 21.10.2014.
 */

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

object TraverserHelperMacros {
  def build[T, A](f: Any , objs: Any*): (T => Option[(T, A)]) = macro TraverserBuilder.buildImpl[T, A]

}

class TraverserBuilder(val c: Context) {
  import c.universe._


  def buildImpl[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree, objs: c.Tree*): c.Tree = {
    val parameter = TermName(c.freshName)
    val cases = buildCases[T, A](f, objs.toList, parameter)
    buildFuncWith[T, A](cases, parameter)
  }

  def buildFuncWith[T : c.WeakTypeTag, A : c.WeakTypeTag](cases: List[c.Tree], parameter: TermName): c.Tree = {
    q"""
        ($parameter: ${implicitly[c.WeakTypeTag[T]]}) => $parameter match {
          case ..$cases
          case v => Some((v, implicitly[Monoid[${implicitly[c.WeakTypeTag[A]]}]].zero))
        }
    """
  }

  def buildImplDelegate[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree, objs: c.Tree*): c.Tree = {
    val parameter = c.internal.enclosingOwner.asMethod.paramLists.head.head.name.toTermName //LOL
    val cases = buildCases[T, A](f, objs.toList, parameter)
    buildDelegateWith[T, A](cases, parameter)
  }

  def buildDelegateWith[T : c.WeakTypeTag, A : c.WeakTypeTag](cases: List[c.Tree], parameter: TermName): c.Tree = {
    q"""
      $parameter match {
        case ..$cases
        case v => Some((v, implicitly[Monoid[${implicitly[c.WeakTypeTag[A]]}]].zero))
      }
     """
  }

  def buildCases[T : c.WeakTypeTag, A : c.WeakTypeTag]
                (f: c.Tree, objs: List[c.Tree],
                 parameter: TermName): List[c.Tree] =
    for {
      obj <- objs
      args <- getParamsWithTypes(obj.symbol.info)
      extractorVars = args._1.map(p => pq"$p @ _")
      pat = pq"$obj(..${extractorVars})"
      stat <- caseMatch[T, A](f, obj, parameter, args._1, args._2)
    } yield cq"$pat => $stat"


  def getParamsWithTypes(typ: c.Type): Option[(List[TermName], List[c.Type])] = {
    val unapply = typ.decl(TermName("unapply"))
    val MethodType(_, resultType) = unapply.typeSignatureIn(typ)
    /* The result type of unapply is Option[T] where T can be a TupleX containing the types by which we will
     * pattern match and construct the type with*/
    if (!resultType.typeArgs.isEmpty){
      val tupleOrNot: c.Type = resultType.typeArgs.head

      val types = if (tupleOrNot.typeConstructor.toString.startsWith("Tuple") /*pretty lame*/) tupleOrNot.typeArgs
                  else List(tupleOrNot)   /*This is disgusting, God please give me a better way to do it*/
      val newNames = types.map(_ => TermName(c.freshName))
      Some((newNames, types))
    }
    else {
      val apply = typ.decl(TermName("apply"))
      val MethodType(args, _) = apply.typeSignatureIn(typ)
      if (!args.isEmpty){
        val newNames = args.map(_ => TermName(c.freshName))
        Some((newNames, args.map(_.info)))
      }
      else
        None
    }
  }

  def createEnum[T : c.WeakTypeTag, A : c.WeakTypeTag]
                (f: c.Tree, name: TermName, typ: c.Type)/*: List[Option[(TermName, TermName, c.Tree)]] */= {
    val aTpe = implicitly[c.WeakTypeTag[A]]
    val newTree = TermName(c.freshName)
    val newResult = TermName(c.freshName)
    val lhs = pq"($newTree: $typ, $newResult @ _)"
    val rhs = typ match {
      case t if t <:< weakTypeOf[T] =>
        Some(q"$f($name)")
      case t if t <:< weakTypeOf[scala.collection.immutable.Seq[T]] =>
        Some(q"_root_.tql.TraverserHelper.traverseSeq($f, $name)")
      case t if t <:< weakTypeOf[scala.collection.immutable.Seq[scala.collection.immutable.Seq[T]]] =>
        Some(q"_root_.tql.TraverserHelper.traverseSeqofSeq($f, $name)")
      case t if t <:< weakTypeOf[scala.Option[T]] =>
        Some(q"_root_.tql.TraverserHelper.optional($f, $name)")
      case _ => None
    }
    rhs.map(r => (newTree, newResult, fq"$lhs <- $r"))
  }


  def caseMatch[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree, constructor: c.Tree, origin: TermName,
                                                      names: List[TermName], types: List[c.Type]): Option[c.Tree] = {

    val parameters: List[(TermName, Type)] = names.zip(types)
    val enums = parameters.map(x => createEnum[T, A](f, x._1, x._2))
    val forEnums: List[c.Tree] = enums.flatMap(_.map(_._3))
    val results: List[TermName] = enums.flatMap(_.map(_._2))

    if (!results.isEmpty){
      val addResults = results.tail.foldLeft[c.Tree](q"${results.head}")((a, b) => q"$a + $b")
      val paramsWithNewParams = parameters.unzip._1.zip(enums.map(_.map(_._1)))
      val reconstructParams = paramsWithNewParams.map(x => x._2.getOrElse(x._1))
      val reconstruct = q"$constructor(..$reconstructParams)"
      val eqList = paramsWithNewParams.foldLeft[c.Tree](q"true"){
        (acc, x) => q"$acc && ${x._2.map(y => q"($y eq ${x._1})").getOrElse(q"true")}"}

      val doesReconstruct = q"if ($eqList) $origin else $reconstruct"

      Some(q"""
        for (
          ..$forEnums
        ) yield ($doesReconstruct, $addResults)
      """)
    }
    else
      None
  }
}
