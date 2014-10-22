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


  //def fp[T <: Tree, A](t: Tree): Option[(Tree, A)] = None

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

  def optional[T <: Tree: ClassTag, A: Monoid](f: TreeMapper[T], a: Option[T])/*: Option[Option[(T, A)]]*/ = Some(a
    .flatMap(f(_))
    .collect{case (x: T, y) if classTag[T].runtimeClass.isInstance(x) => (Some(x), y)}
    .getOrElse((None, implicitly[Monoid[A]].zero)))


  def traverse[A : Monoid](tree: Tree, f: TreeMapper[A]): MatcherResult[A] = {
    val m = implicitly[Monoid[A]]
    import scala.collection.immutable._

    def traverseSeq[T <: Tree: ClassTag](seq: Seq[T]): Option[(Seq[T], A)] = {
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

    /*Code duplication \0/ !!*/
    def traverseSeqofSeq[T <: Tree: ClassTag](seq: Seq[Seq[T]]): Option[(Seq[Seq[T]], A)] = {
      var buffer = new ListBuffer[Seq[T]]()
      var hasChanged = false
      var acc = m.zero
      for (t <- seq){
        traverseSeq(t) match {
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

    @inline
    def optional[A <: Tree: ClassTag](a: Option[A]) = Some(a
      .flatMap(f(_))
      .collect{case (x: A, y) if classTag[A].runtimeClass.isInstance(x) => (Some(x), y)}
      .getOrElse((None, m.zero)))

    @inline
    def traverse1[A <: Tree : ClassTag, T <: Tree](a: A, callback: A => T) = for {
      (a1: A, a2) <- f(a)
      if classTag[A].runtimeClass.isInstance(a1)
    } yield (if ((a eq a1)) tree else callback(a1), a2)

    @inline
    def traverse1s[A <: Tree : ClassTag, T <: Tree](a: Seq[A], callback: Seq[A] => T) = for {
      (a1, a2) <- traverseSeq(a)
    } yield (if ((a eq a1)) tree else callback(a1), a2)

    @inline
    def traverse2[A <: Tree : ClassTag, B <: Tree : ClassTag, T <: Tree](a: A, b: B, callback: (A, B) => T) = for {
      (a1: A, a2) <- f(a)
      if classTag[A].runtimeClass.isInstance(a1)
      (b1: B, b2) <- f(b)
      if classTag[B].runtimeClass.isInstance(b1)
    } yield (if ((a eq a1) && (b eq b1)) tree else callback(a1, b1), a2 + b2)

    @inline
    def traverse2st[A <: Tree : ClassTag, B <: Tree : ClassTag, T <: Tree](a: Seq[A], b: B, callback: (Seq[A], B) => T) = for {
      (a1, a2) <- traverseSeq(a)
      (b1: B, b2) <- f(b)
      if classTag[B].runtimeClass.isInstance(b1)
    } yield (if ((a eq a1) && (b eq b1)) tree else callback(a1, b1), a2 + b2)

    @inline
    def traverse2ts[A <: Tree : ClassTag, B <: Tree : ClassTag, T <: Tree](a: A, b: Seq[B], callback: (A, Seq[B]) => T) = for {
      (a1: A, a2) <- f(a)
      if classTag[A].runtimeClass.isInstance(a1)
      (b1, b2) <- traverseSeq(b)
    } yield (if ((a eq a1) && (b eq b1)) tree else callback(a1, b1), a2 + b2)

    @inline
    def traverse3sst[A <: Tree: ClassTag, B <: Tree: ClassTag, C <: Tree : ClassTag, T <: Tree](a: Seq[A], b: Seq[B], c: C, cb: (Seq[A], Seq[B], C) => T) = for {
      (a1, a2) <- traverseSeq(a)
      (b1, b2) <- traverseSeq(b)
      (c1: C, c2) <- f(c)
    } yield (if ((a eq a1) && (b eq b1) && (c eq c1)) tree else cb(a1, b1, c1), a2 + b2 + c2)


    /*********************/

    def termMatcher(t: Term): MatcherResult[A] = t match {
      case Term.Select(qual, selector) => traverse1(qual, (a: Qual.Term) => Term.Select(a, selector))
      case Term.Interpolate(prefix, parts, args) => traverse1s(args, (a: Seq[Term]) => Term.Interpolate(prefix, parts, a))

      case Term.Apply(fun, args) => traverse2ts(fun, args, (a: Term, b: Seq[Arg]) => Term.Apply(a,b))
      case Term.ApplyType(fun, args) => traverse2ts(fun, args, (a: Term, b: Seq[Type]) => Term.ApplyType(a,b))
      case Term.ApplyInfix(lhs, op, targs, args) => for {
        (a1: Term, a2) <- f(lhs)
        (b1, b2) <- traverseSeq(targs)
        (c1, c2) <- traverseSeq(args)
      } yield (Term.ApplyInfix(a1, op, b1, c1), a2 + b2 + c2)
      case Term.ApplyUnary(op, arg) => traverse1(arg, (a: Term) => Term.ApplyUnary(op, a))

      case Term.Assign(lhs, rhs) => traverse2(lhs, rhs, (a: Term.Ref, b: Term) => Term.Assign(a,b))
      case Term.Update(lhs, rhs) => traverse2(lhs, rhs, (a: Term.Apply, b: Term) => Term.Update(a,b))
      case Term.Return(expr) => traverse1(expr,(a: Term) => Term.Return(a))
      case Term.Throw(expr) => traverse1(expr,(a: Term) => Term.Throw(a))
      case Term.Ascribe(expr, tpe) => traverse2(expr, tpe, (a: Term, b: Type) => Term.Ascribe(a,b))
      case Term.Annotate(expr,annots) => traverse2ts(expr, annots, (a: Term, b: Seq[Mod.Annot]) => Term.Annotate(a, b))

      case Term.Tuple(elements) => traverse1s(elements, (a: Seq[Term]) => Term.Tuple(a))
      case Term.Block(elements) => traverse1s(elements, (a: Seq[Stmt.Block]) => Term.Block(a))

      case Term.If(cond, thenp, elsep) => (for {
        (a1: Term, a2) <- f(cond)
        (b1: Term, b2) <- f(thenp)
        (c1: Term, c2) <- f(elsep)
      } yield (if ((a1 eq cond) && (b1 eq thenp) && (c1 eq elsep)) t else Term.If(a1, b1, c1), a2 + b2 + c2))

      case Term.Match(scrut, cases) => traverse2(scrut, cases, (a: Term, b: Term.Cases) => Term.Match(a,b))
      case Term.Try(expr, catchp, finallyp) => for {
        (a1: Term, a2) <- f(expr)
        (b1, b2) <- optional(catchp)
        (c1, c2) <- optional(finallyp)
      } yield (Term.Try(a1, b1, c1), a2 + b2 + c2)
      case Term.Function(params,body) => traverse2st(params, body, (a: Seq[Param], b: Term) => Term.Function(a, b))

      case Term.Cases(aux) => traverse1s(aux, (a: Seq[Aux.Case]) => Term.Cases(a))

      case Term.While(expr, body) => traverse2(expr, body, (a: Term, b: Term) => Term.While(a, b))
      case Term.Do(body,expr) => traverse2(body, expr, (a: Term, b: Term) => Term.Do(a, b))
      case Term.For(enums,body) => traverse2st(enums, body, (a: Seq[Enum], b: Term) => Term.For(a, b))
      case Term.ForYield(enums,body) => traverse2st(enums, body, (a: Seq[Enum], b: Term) => Term.ForYield(a, b))
      //@ast class New(templ: Aux.Template) extends Term ???
      case Term.Eta(term) => traverse1(term, (a: Term) => Term.Eta(a))
      case v => Some((v, m.zero))
    }
                 /*
    def typeMatcher(t: Type): MatcherResult[Tree, A] = t match{
      case Type.Select(qual, selector) => traverse1(qual, (a: Qual.Type) => Type.Select(a, selector))
      case Type.Project(qual, selector) => traverse1(qual, (a: Type) => Type.Project(a, selector))
      case Type.Singleton(ref) => traverse1(ref, (a: Term.Ref) => Type.Singleton(a))
      //@ast class Placeholder(bounds: Aux.TypeBounds) extends Type ??
      case Type.Apply(tpe, args) => traverse2ts(tpe, args, (a: Type, b: Seq[Type]) => Type.Apply(a, b))
      case Type.ApplyInfix(lhs, op, rhs) => traverse2(lhs, rhs, (a: Type, b: Type) => Type.ApplyInfix(a, op, b))
      case Type.Function(params,body) => traverse2st(params, body, (a: Seq[Param.Type], b: Type) => Type.Function(a, b))
      case Type.Tuple(elements) => traverse1s(elements, (a: Seq[Type]) => Type.Tuple(a))
      case Type.Compound(tpes, refinement) => for {
        (a1, a2) <- traverseSeq(tpes)
        (b1, b2) <- traverseSeq(refinement)
      } yield (Type.Compound(a1, b1), a2 + b2)
      case Type.Existential(tpe, quants) => traverse2ts(tpe, quants, (a: Type, b: Seq[Stmt.Existential]) => Type.Existential(a, b))
      case Type.Annotate(tpe, annots) => traverse2ts(tpe, annots, (a: Type, b: Seq[Mod.Annot]) => Type.Annotate(a, b))
      case v => Some((v, m.zero))
    }

    def patMatcher(t: Pat): MatcherResult[Tree, A] = t match {
      case Pat.Bind(lhs, rhs) => traverse1(rhs, (a: Pat) => Pat.Bind(lhs, a))
      case Pat.Alternative(lhs, rhs) => traverse2(lhs, rhs, (a: Pat, b: Pat) => Pat.Alternative(a, b))
      case Pat.Tuple(elements) => traverse1s(elements, (a: Seq[Pat]) => Pat.Tuple(a))
      case Pat.Extract(ref, targs, elements) => for {
        (a1: Term.Ref, a2) <- f(ref)
        (b1, b2) <- traverseSeq(targs)
        (c1, c2) <- traverseSeq(elements)
      } yield (Pat.Extract(a1, b1, c1), a2 + b2 + c2)
      case Pat.ExtractInfix(lhs, ref, rhs) => traverse2ts(lhs, rhs, (a: Pat, b: Seq[Pat]) => Pat.ExtractInfix(a, ref, b))
      case Pat.Interpolate(prefix, parts, args) => traverse1s(args, (a: Seq[Pat]) => Pat.Interpolate(prefix, parts, a))
      case Pat.Typed(lhs, rhs) => traverse2(lhs, rhs, (a: Pat, b: Type) => Pat.Typed(a, b))
      case v => Some((v, m.zero))
    }

    def declMatcher(t: Decl): MatcherResult[Tree, A] = t match {
      case Decl.Val(mods, pats, decltpe) => traverse3sst(mods, pats, decltpe, (a: Seq[Mod], b: Seq[Term.Name], c: meta.Type) => Decl.Val(a, b, c))
      case Decl.Var(mods, pats, decltpe) => traverse3sst(mods, pats, decltpe, (a: Seq[Mod], b: Seq[Term.Name], c: meta.Type) => Decl.Var(a, b, c))
      /*
      Def(mods: Seq[Mod],
          name: Term.Name,
          tparams: Seq[TypeParam],
          explicits: Seq[Seq[Param.Named]],
          implicits: Seq[Param.Named],
          decltpe: meta.Type)
      */
      /*
      Procedure(mods: Seq[Mod],
                name: Term.Name,
                tparams: Seq[TypeParam],
                explicits: Seq[Seq[Param.Named]],
                implicits: Seq[Param.Named])
      */
      case Decl.Type(mods, name, tparams, bounds) => traverse3sst(mods, tparams, bounds,
        (a: Seq[Mod], b: Seq[TypeParam], c: Aux.TypeBounds) => Decl.Type(a, name, b, c))
      case v => Some((v, m.zero))
    }

    def defnMatcher(t: Defn): MatcherResult[Tree, A] = t match {
      case Defn.Val(mods, pats, decltpe, rhs) => for {
        (a1, a2) <- traverseSeq(mods)
        (b1, b2) <- traverseSeq(pats)
        (c1, c2) <- optional(decltpe)
        (d1: Term, d2) <- f(rhs)
      } yield (if ((a1 eq mods) && (b1 eq pats) && (c1 eq decltpe) && (d1 eq rhs)) t else Defn.Val(a1, b1, c1, d1),
          a2 + b2 + c2 + d2)
      case Defn.Var(mods, pats, decltpe, rhs) => for {
        (a1, a2) <- traverseSeq(mods)
        (b1, b2) <- traverseSeq(pats)
        (c1, c2) <- optional(decltpe)
        (d1, d2) <- optional(rhs)
      } yield (if ((a1 eq mods) && (b1 eq pats) && (c1 eq decltpe) && (d1 eq rhs)) t else Defn.Var(a1, b1, c1, d1),
          a2 + b2 + c2 + d2)
      case v => Some((v, m.zero))
    }

    def ctorMatcher(t: Ctor): MatcherResult[Tree, A] = t match {
      case Ctor.Primary(mods, explicits, implicits) => for {
        (a1, a2) <- traverseSeq(mods)
        (b1, b2) <- traverseSeqofSeq(explicits)
        (c1, c2) <- traverseSeq(implicits)
      } yield (if ((a1 eq mods) && (b1 eq explicits) && (c1 eq implicits)) t else Ctor.Primary(a1, b1, c1), a2 + b2 + c2)
      case Ctor.Secondary(mods, explicits, implicits, primaryCtorArgss, stats) => for {
        (a1, a2) <- traverseSeq(mods)
        (b1, b2) <- traverseSeqofSeq(explicits)
        (c1, c2) <- traverseSeq(implicits)
        (d1, d2) <- traverseSeqofSeq(primaryCtorArgss)
        (e1, e2) <- traverseSeq(stats)
      } yield ( if ((a1 eq mods) && (b1 eq explicits) && (c1 eq implicits) && (d1 eq primaryCtorArgss) && (e1 eq stats)) t
        else Ctor.Secondary(a1, b1, c1, d1, e1),
          a2 + b2 + c2 + d2 + e2)
    }


    def paramMatcher(t: Param): MatcherResult[Tree, A] = t match {
      case Param.Anonymous(mods, decltpe) => for {
        (a1, a2) <- traverseSeq(mods)
        (b1, b2) <- optional(decltpe)
      } yield (if ((a1 eq mods) && (b1 eq decltpe)) t else Param.Anonymous(a1, b1), a2 + b2)
      case Param.Named(mods, name, decltpe, default) => for {
        (a1, a2) <- traverseSeq(mods)
        (b1, b2) <- optional(decltpe)
        (c1, c2) <- optional(default)
      } yield (if ((a1 eq mods) && (b1 eq decltpe) && (c1 eq default)) t else Param.Named(a1, name, b1, c1), a2 + b2 + c2)
    }

    def typeParamMatcher(t: TypeParam): MatcherResult[Tree, A] = t match {
      case TypeParam.Anonymous(mods, tparams, contextBounds, viewBounds, bounds) => for {
        (a1, a2) <- traverseSeq(mods)
        (b1, b2) <- traverseSeq(tparams)
        (c1, c2) <- traverseSeq(contextBounds)
        (d1, d2) <- traverseSeq(viewBounds)
        (e1: Aux.TypeBounds, e2) <- f(bounds)
      } yield (if ((a1 eq mods) && (b1 eq tparams) && (c1 eq contextBounds) && (d1 eq viewBounds) && (e1 eq bounds)) t
        else TypeParam.Anonymous(a1, b1, c1, d1, e1),
          a2 + b2 + c2 + d2 + e2)
      case TypeParam.Named(mods, name, tparams, contextBounds, viewBounds, bounds) => for {
        (a1, a2) <- traverseSeq(mods)
        (b1, b2) <- traverseSeq(tparams)
        (c1, c2) <- traverseSeq(contextBounds)
        (d1, d2) <- traverseSeq(viewBounds)
        (e1: Aux.TypeBounds, e2) <- f(bounds)
      } yield (if ((a1 eq mods) && (b1 eq tparams) && (c1 eq contextBounds) && (d1 eq viewBounds) && (e1 eq bounds)) t
        else TypeParam.Named(a1, name, b1, c1, d1, e1),
          a2 + b2 + c2 + d2 + e2)
    }

    def argMatcher(t: Arg): MatcherResult[Tree, A] = t match {
      case Arg.Named(name, rhs) => traverse1(rhs, (a: Term) => Arg.Named(name, a))
      case Arg.Repeated(arg) => traverse1(arg, (a: Term) => Arg.Repeated(a))
    }

    def enumMatcher(t: Enum): MatcherResult[Tree, A] = t match {
      case Enum.Generator(pat, rhs) => traverse2(pat, rhs, (a: Pat, b: Term) => Enum.Generator(a, b))
      case Enum.Val(pat, rhs) => traverse2(pat, rhs, (a: Pat, b: Term) => Enum.Val(a, b))
      case Enum.Guard(cond) => traverse1(cond, (a: Term) => Enum.Guard(a))
    }

    def auxMatcher(t: Tree): MatcherResult[Tree, A] = t match {
      case Aux.CompUnit(stats) => traverse1s(stats, (a: Seq[Stmt.TopLevel]) => Aux.CompUnit(a))
      case Aux.Case(pat, cond, stats) => for {
        (a1: Pat, a2) <- f(pat)
        (b1, b2) <- optional(cond)
        (c1, c2) <- traverseSeq(stats)
      } yield (if ((a1 eq pat) && (b1 eq cond) && (c1 eq stats)) t else Aux.Case(a1, b1, c1), a2 + b2 + c2)
      case Aux.Parent(tpe, argss) => for {
        (a1: Type, a2) <- f(tpe)
        (b1, b2) <- traverseSeqofSeq(argss)
      } yield (if ((a1 eq tpe) && (b1 eq argss)) t else Aux.Parent(a1, b1), a2 + b2)
      case Aux.Template(early, parents, self, stats) => for {
        (a1, a2) <- traverseSeq(early)
        (b1, b2) <- traverseSeq(parents)
        (c1: Aux.Self, c2) <- f(self)
        (d1, d2) <- traverseSeq(stats)
      } yield (if ((a1 eq early) && (b1 eq parents) && (c1 eq self) && (d1 eq stats)) t
        else Aux.Template(a1, b1, c1, d1),
          a2 + b2 + c2 + d2)
      case Aux.Self(name, decltpe) => for {
        (a1, a2) <- optional(name)
        (b1, b2) <- optional(decltpe)
      } yield (if ((a1 eq name) && (b1 eq decltpe)) t else Aux.Self(name, decltpe), a2 + b2)
      case Aux.TypeBounds(lo, hi) => traverse2(lo, hi, (a: Type, b: Type) => Aux.TypeBounds(a, b))
      case v => Some((v, m.zero))
    }    */

    def testTermMatcher = TraverserHelper.build[Tree,A](f,
      Term.If, Term.Block, Term.ApplyInfix)


    (tree match {
      case t: Term => testTermMatcher(t)
      case v => Some((v, m.zero))
      /*case t: Type => typeMatcher(t)
      case t: Pat => patMatcher(t)
      case t: Decl => declMatcher(t)
      case t: Defn => defnMatcher(t)
      case t: Ctor => ctorMatcher(t)
      case t: Param => paramMatcher(t)
      case t: TypeParam => typeParamMatcher(t)
      case t: Arg => argMatcher(t)
      case t: Enum => enumMatcher(t)
      case Param.Type.ByName(tpe) => traverse1(tpe, (a: Param.Type) => Param.Type.ByName(a))
      case Param.Type.Repeated(tpe) => traverse1(tpe, (a: Param.Type) => Param.Type.Repeated(a))
      case Pkg(ref, stats) => traverse2ts(ref, stats, (a: Term.Ref, b: Seq[Stmt.TopLevel]) => Pkg(a, b))
      case Import.Clause(ref, sels) => traverse2ts(ref, sels, (a: Term.Ref, b: Seq[Selector]) => Import.Clause(a, b))
      case Import.Rename(from, to) => traverse2(from, to, (a: Import.Name, b: Import.Name) => Import.Rename(a, b))
      case Import.Unimport(name) => traverse1(name, (a: Import.Name) => Import.Unimport(a))
      case t => auxMatcher(t)*/
    })
  }
}
