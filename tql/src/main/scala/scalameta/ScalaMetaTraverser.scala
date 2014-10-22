package scalameta

/**
 * Created by Eric on 20.10.2014.
 */

/**
 * Created by Eric on 19.10.2014.
 */

import tql._
import scala.collection.immutable.Seq
import scala.collection.mutable.ListBuffer
import scala.meta.Import.Selector
import scala.meta._

object ScalaMetaTraverser  extends Traverser[Tree] with Combinators[Tree] with SyntaxEnhancer[Tree] {

  import MonoidEnhencer._
  import scala.reflect.{ClassTag, classTag}

  implicit object tree2tree extends AllowedTransformation[Tree, Tree]


  def traverseSeq[T <: Tree: ClassTag, A : Monoid](f: TreeMapper[A], seq: Seq[T]): Option[(Seq[T], A)] = {
    val m = implicitly[Monoid[A]]
    var buffer = new ListBuffer[T]()
    var hasChanged = false
    var acc = m.zero
    for (t <- seq){
      f(t) match {
        case Some((a1: T, a2)) if classTag[T].runtimeClass.isInstance(a1) =>
          buffer.append(a1)
          acc += a2
          hasChanged |= !(a1 eq t)
        case _ =>
          hasChanged = true
      }
    }
    Some((if (hasChanged) collection.immutable.Seq(buffer: _*) else seq, acc))
  }

  def traverseSeqofSeq[T <: Tree: ClassTag, A : Monoid](f: TreeMapper[A], seq: Seq[Seq[T]]): Option[(Seq[Seq[T]], A)] = {
    val m = implicitly[Monoid[A]]
    var buffer = new ListBuffer[Seq[T]]()
    var hasChanged = false
    var acc = m.zero
    for (t <- seq){
      traverseSeq(f, t) match {
        case Some((a1, a2))=>
          buffer.append(a1)
          acc += a2
          hasChanged |= !(a1 eq t)
        case _ =>
          hasChanged = true
      }
    }
    Some((if (hasChanged) collection.immutable.Seq(buffer: _*) else seq, acc))
  }

  def optional[T <: Tree: ClassTag, A: Monoid](f: TreeMapper[A], a: Option[T])/*: Option[Option[(T, A)]]*/ = Some(a
    .flatMap(f(_))
    .collect{case (x: T, y) if classTag[T].runtimeClass.isInstance(x) => (Some(x), y)}
    .getOrElse((None, implicitly[Monoid[A]].zero)))


  def traverse[A : Monoid](tree: Tree, f: TreeMapper[A]): MatcherResult[A] = {
    val m = implicitly[Monoid[A]]

    def termMatcher = TraverserHelper.build[Tree,A](f,
      Term.This, Term.Name, Term.Select, Term.Interpolate,
      Term.Apply, Term.ApplyType, Term.ApplyInfix, Term.ApplyUnary,
      Term.Assign, Term.Update, Term.Return, Term.Throw, Term.Ascribe, Term.Annotate,
      Term.Tuple, Term.Block, Term.If, Term.Match, Term.Try, Term.Function, Term.Cases ,
      Term.Cases, Term.While, Term.Do, Term.For, Term.ForYield, Term.New, Term.Eta)

    def typeMatcher = TraverserHelper.build[Tree,A](f,
      Type.Select, Type.Project, Type.Singleton,
      Type.Apply, Type.ApplyInfix, Type.Function, Type.Tuple,
      Type.Compound, Type.Existential, Type.Annotate, Type.Placeholder,
      Param.Type.ByName, Param.Type.Repeated
    )

    def patMatcher = TraverserHelper.build[Tree,A](f,
      Pat.Bind, Pat.Alternative, Pat.Tuple, Pat.Extract,
      Pat.ExtractInfix, Pat.Interpolate, Pat.Typed
    )

    def declMatcher = TraverserHelper.build[Tree,A](f,
      Decl.Val, Decl.Var, Decl.Def, Decl.Procedure, Decl.Type
    )

    def defnMatcher = TraverserHelper.build[Tree,A](f,
      Defn.Val, Defn.Var, Defn.Def, Defn.Procedure,
      Defn.Macro, Defn.Type, Defn.Class, Defn.Trait
    )

    def pkgMatcher = TraverserHelper.build[Tree,A](f,Pkg)

    def ctorMatcher = TraverserHelper.build[Tree,A](f,
      Ctor.Primary, Ctor.Secondary
    )

    def importMatcher = TraverserHelper.build[Tree,A](f,
      Import.Clause, Import.Rename, Import.Unimport
    )

    def paramMatcher = TraverserHelper.build[Tree,A](f,
      Param.Anonymous, Param.Named
    )

    def typeParamMatcher = TraverserHelper.build[Tree,A](f,
      TypeParam.Anonymous, TypeParam.Named
    )

    def argMatcher = TraverserHelper.build[Tree,A](f,
      Arg.Named, Arg.Repeated
    )

    def enumMatcher = TraverserHelper.build[Tree,A](f,
      Enum.Generator, Enum.Guard, Enum.Val
    )

    def modMatcher = TraverserHelper.build[Tree,A](f,
      Mod.Annot, Mod.Private, Mod.Protected
    )

    def auxMatcher = TraverserHelper.build[Tree,A](f,
      Aux.CompUnit, Aux.Case, Aux.Parent, Aux.Template,
      Aux.Template, Aux.Self, Aux.TypeBounds,
      Qual.Super
    )


    (tree match {
      case t: Term => termMatcher(t)
      case t: Type => typeMatcher(t)
      case t: Pat => patMatcher(t)
      case t: Decl => declMatcher(t)
      case t: Defn => defnMatcher(t)
      case t: Pkg => pkgMatcher(t)
      case t: Ctor => ctorMatcher(t)
      case t: Import => importMatcher(t)
      case t: Param => paramMatcher(t)
      case t: TypeParam => typeParamMatcher(t)
      case t: Arg => argMatcher(t)
      case t: Enum => enumMatcher(t)
      case t: Mod => modMatcher(t)
      case t => auxMatcher(t)
    })
  }
}
