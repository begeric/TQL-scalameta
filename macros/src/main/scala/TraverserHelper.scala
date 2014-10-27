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

  def buildFromTopSymbol[T, A](f: Any): Any = macro TraverserBuilder.buildFromTopSymbol[T, A]

  def hierarchy[T, A](f: Any): Any = macro TraverserBuilder.hierarchyImpl[T, A]

}

class TraverserBuilder(val c: Context) extends org.scalameta.adt.AdtReflection {
  val u: c.universe.type = c.universe
  import c.universe._


  def buildFromTopSymbol[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree): c.Tree = {


    //c.abort(c.enclosingPosition, show(u.symbolOf[T].asRoot.allLeafs))
    //val allLeafs = u.symbolOf[T].asRoot.allLeafs.map(x => c.typecheck(q"${TermName(x.sym.fullName)}", mode = c.TYPEmode))
    /*val allLeafs = u.symbolOf[T].asRoot.allLeafs.map(x => x.sym.info.companion.decl(u.TermName("unapply")))
    val allLeafsConstructors = u.symbolOf[T].asRoot.allLeafs.map(x => c.typecheck(q"${TermName(x.sym.fullName)}", mode = c.TYPEmode).tpe.member(TermName("unapply"))  )  */
    //c.abort(c.enclosingPosition, show(allLeafParams))
    //val allLeafs = u.symbolOf[T].asRoot.allLeafs.map(x => getParamsWithSymbol(x.sym))
    //c.abort(c.enclosingPosition, show(allLeafs))
    q""
    //buildImpl[T, A](f, q"..$allLeafs")
  }



  def hierarchyImpl[T : c.WeakTypeTag, A: c.WeakTypeTag](f: c.Tree) = {
    /*val m = implicitly[c.WeakTypeTag[T]]
    val tpeSym = m.tpe.typeSymbol

    def patmatFromSymbol(sym: ClassSymbol, scrut: TermName): Option[c.Tree] = {
      val subClasses = sym.knownDirectSubclasses
      val cases = for {
        s <- subClasses
        if s.isClass
        tmpName = TermName(c.freshName)
        mat <- patmatFromSymbol(s.asClass, tmpName)
      } yield cq"$tmpName: $s => $mat"

      if (!cases.isEmpty) {
        Some(q"""
            $scrut match {
              case ..$cases
            }
        """)
      }
      else if (!subClasses.isEmpty){
        c.abort(c.enclosingPosition, show(subClasses.map(x => c.typecheck(q"${x.companion}").tpe.member(TermName("unapply")))))
        val cases = buildCases[T, A](f, subClasses.map(x => c.typecheck(q"${x.companion}")).toList, scrut)
        if (cases.isEmpty)
          None
        else {
        //c.abort(c.enclosingPosition, show(cases))
          Some(q"""
              $scrut match {
                case ..$cases
                case v => Some((v, implicitly[Monoid[${implicitly[c.WeakTypeTag[A]]}]].zero))
              }
          """)
        }
      }
      else
        None

    }
    val tmpName = TermName(c.freshName)
    val tmp = patmatFromSymbol(tpeSym.asClass, tmpName)
    q"${show(tmp)}"        */
    q""
  }

  def buildImpl[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree, cases: c.Tree*): c.Tree = {
    val parameter = TermName(c.freshName)

    val casesImpl = buildCases[T, A](f, cases.toList, parameter)

    q"""
        ($parameter: ${implicitly[c.WeakTypeTag[T]]}) => $parameter match {
          case ..$casesImpl
          case v => Some((v, implicitly[Monoid[${implicitly[c.WeakTypeTag[A]]}]].zero))
        }
    """
  }

  def buildPatMat[T : c.WeakTypeTag, A : c.WeakTypeTag](scrut: c.Tree, cases: c.Tree): c.Tree =
    q"""
        $scrut match {
          case ..$cases
        }
    """

  def buildCases[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree, cases: List[c.Tree], parameter: TermName): List[c.Tree] =
    for {
      cas <- cases
      argsList <- getParamsWithTypes(cas.symbol.info)
      parametersList = argsList._1.map(p => pq"$p @ _")
      extractor = pq"$cas(..${parametersList})"
      matched <- caseMatch[T, A](f, cas, parameter, argsList._1, argsList._2)
    } yield cq"$extractor => $matched"


  def getParamsWithTypes(obj: c.Type): Option[(List[TermName], List[c.Type])] = {
    val f = obj.decl(TermName("unapply"))
    val MethodType(_, resultType) = f.typeSignatureIn(obj)
    if (!resultType.typeArgs.isEmpty){
      val tupleOrNot: c.Type = resultType.typeArgs.head

      //pretty lame
      val types = if (tupleOrNot.typeConstructor.toString.startsWith("Tuple")) tupleOrNot.typeArgs
      else List(tupleOrNot)   /*This is disgusting, God please give me a better way to do it*/
      val newNames = types.map(_ => TermName(c.freshName))
      Some((newNames, types))
    }
    else
      None
  }

  /*def getParamsWithTypes(obj: c.Tree): Option[(List[TermName], List[c.Type])] = {
    //c.abort(c.enclosingPosition, show(obj.symbol.info.decl(TermName("unapply"))))
    val f = obj.tpe.member(TermName("unapply"))
    val MethodType(_, resultType) = f.typeSignatureIn(obj.tpe)
    if (!resultType.typeArgs.isEmpty){
      val tupleOrNot: c.Type = resultType.typeArgs.head

      //pretty lame
      val types = if (tupleOrNot.typeConstructor.toString.startsWith("Tuple")) tupleOrNot.typeArgs
                  else List(tupleOrNot)   /*This is disgusting, God please give me a better way to do it*/
      val newNames = types.map(_ => TermName(c.freshName))
      Some((newNames, types))
    }
    else
      None
  }*/

  /*def getParamsWithSymbol(sym: c.Symbol): Option[(List[TermName], List[c.Type])] = {
    val f = sym.info.companion.decl(TermName("unapply"))
    val MethodType(_, resultType) = f.typeSignatureIn(sym.info)
    if (!resultType.typeArgs.isEmpty){
      val tupleOrNot: c.Type = resultType.typeArgs.head

      //pretty lame
      val types = if (tupleOrNot.typeConstructor.toString.startsWith("Tuple")) tupleOrNot.typeArgs
      else List(tupleOrNot)   /*This is disgusting, God please give me a better way to do it*/
      val newNames = types.map(_ => TermName(c.freshName))
      Some((newNames, types))
    }
    else
      None
  }  */


  def createEnum[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree, name: TermName, typ: c.Type) = {
    val aTpe = implicitly[c.WeakTypeTag[A]]
    val newTree = TermName(c.freshName)
    val newResult = TermName(c.freshName)
    val lhs = pq"($newTree: $typ, $newResult @ _)"
    val rhs = typ match {
      case t if t <:< weakTypeOf[T] =>
        Some(q"$f($name)")
      case t if t <:< weakTypeOf[scala.collection.immutable.Seq[T]] =>
        Some(q"scalameta.ScalaMetaTraverser.traverseSeq($f, $name)")
      case t if t <:< weakTypeOf[scala.collection.immutable.Seq[scala.collection.immutable.Seq[T]]] =>
        Some(q"scalameta.ScalaMetaTraverser.traverseSeqofSeq($f, $name)")
      case t if t <:< weakTypeOf[scala.Option[T]] =>
        Some(q"scalameta.ScalaMetaTraverser.optional($f, $name)")
      case _ => None
    }
    rhs.map(r => (newTree, newResult, fq"$lhs <- $r"))
  }


  def caseMatch[T : c.WeakTypeTag, A : c.WeakTypeTag](func: c.Tree, first: c.Tree, origin: TermName,
                                                      names: List[TermName], types: List[c.Type]): Option[c.Tree] = {

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
