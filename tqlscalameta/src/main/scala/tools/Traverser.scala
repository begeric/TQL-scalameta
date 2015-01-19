package tools
/**
 * Created by Eric on 25.11.2014.
 */

import scala.meta.Tree
import scala.meta._
import org.scalameta.adt._


/**
 * Macro generated traversers
 * On the way of finding the most efficient one
 * */


class Traverser{
	def traverse(tree: Tree):Unit = TraverserBuilder.traverseAdt[Tree]
}

/*class TraverserBranchesFirst{
  def traverse(tree: Tree):Unit = TraverserBuilder.traverseAdtBranchesFirst[Tree]
}  */

class TraverserTableTag{

  val myTable = TraverserTableTag.buildTraverseTable

  def traverse(tree: Tree): Unit = myTable(tree.$tag)(tree, traverse _)
}

object TraverserTableTag {
  def buildTraverseTable: Array[(Tree, Tree => Unit) => Unit] = TraverserBuilder.buildTraverseTable[Tree]
}

class TraverserTableTagFunc {
  val myTable = TraverserTableTagFunc.buildTraverseTable(traverse)
  def traverse(tree: Tree): Unit = myTable(tree.$tag)(tree)
}

object TraverserTableTagFunc {
  def buildTraverseTable(f: Tree => Unit): Array[Tree => Unit] = TraverserBuilder.buildTraverseTableWithMethod[Tree](f)
}

/*Change the order of the pattern mathc, as in the scalac traverser*/
class OptimzedOrderTraverser extends TraverserTableTag {
  import scala.meta.internal.ast._

  def traverseType(tree: Type): Unit =  tree match {
    case  _ : Type.Name |_: Type.Apply =>
    case _ => super.traverse(tree)
  }

  def traverseTerm(tree: Term): Unit = tree match {
    case _: Term.Name =>
    case Term.ApplyInfix(lhs, op, targs, args) =>
      traverseTerm(lhs)
      traverseTerm(op)
      targs.foreach(traverseType(_))
      args.foreach(traverse(_))
    case Term.Select(qual, selector) =>
      traverseTerm(qual)
      traverseTerm(selector)
    case Term.Match(scrut, cases) =>
      traverseTerm(scrut)
      cases.foreach(traverse(_))
    case _ => super.traverse(tree)
  }

  override def traverse(tree: scala.meta.Tree) = tree match {
    case _: Term.Name | _ : Lit.Char | _ : Lit.Int | _ : Type.Name | _: Term.Param | _: Type.Apply =>
    case _ : Pat.Wildcard =>
    case Case(pat, conds, stats) =>
      traverse(pat)
      conds.foreach(traverseTerm(_))
      stats.foreach(traverse(_))
    case Pat.Extract(ref, targs, elements) =>
      traverseTerm(ref)
      targs.foreach(traverse(_))
      elements.foreach(traverse(_))
    case _ => super.traverse(tree)
  }
}

class HandWrittenScalaMeta{
  import scala.meta.internal.ast._

  def traverse(tree: meta.Tree): Unit = tree match {
    case _: Term.Name | _: Lit.Char =>
    case Term.Apply(fun, args) =>
      traverse(fun)
      args.foreach(x => traverse(x))
    case _: Type.Name =>
    case Type.Param(mods, name , tparams , contextBounds , viewBounds , typeBounds) =>
      mods.foreach(x => traverse(x))
      name.foreach(x => traverse(x))
      tparams.foreach(x => traverse(x))
      viewBounds.foreach(x => traverse(x))
      traverse(typeBounds)
    case Type.Apply(t, args) =>
      traverse(t)
      args.foreach(x => traverse(x))
    case Term.ApplyInfix(lhs, op , targs , args) =>
      traverse(lhs)
      traverse(op)
      targs.foreach(x => traverse(x))
    case Term.Select(a, b) =>
      traverse(a)
      traverse(b)
    case Defn.Def(mods, name : scala.meta.internal.ast.Term.Name, tparams , paramss, decltpe, body) =>
      mods.foreach(x => traverse(x))
      traverse(name)
      tparams.foreach(x => traverse(x))
      paramss.foreach(x => x.foreach(y => traverse(y)))
      decltpe.foreach(x => traverse(x))
      traverse(body)
    case _: Pat.Wildcard =>
    case Case(pat, cond, stat) =>
      traverse(pat)
      cond.foreach(x => traverse(x))
      stat.foreach(x => traverse(x))
    case _: Lit.String =>
    case Pat.Extract(ref, targs, elements) =>
      traverse(ref)
      targs.foreach(x => traverse(x))
      elements.foreach(x => traverse(x))
    case Term.If(a, b, c) =>
      traverse(a)
      traverse(b)
      traverse(c)
    case Term.Match(struct, cases) =>
      traverse(struct)
      cases.foreach(x => traverse(x))
    case Templ(early, parents, self, stats) =>
      early.foreach(x => traverse(x))
      parents.foreach(x => traverse(x))
      traverse(self)
      stats.foreach(x => traverse(x))
    case Type.Tuple(elems) =>
      elems.foreach(x => traverse(x))
    case Term.Block(stats) =>
      stats.foreach(x => traverse(x))
    case Defn.Val(mods, pats, decltpe, rhs) =>
      mods.foreach(x => traverse(x))
      pats.foreach(x => traverse(x))
      decltpe.foreach(x => traverse(x))
      traverse(rhs)
    case Ctor.Ref(tpe, argss) =>
      traverse(tpe)
      argss.foreach(args => args.foreach(x => traverse(x)))
    case Defn.Class(mods, name, tparams, ctor, templ) =>
      mods.foreach(x => traverse(x))
      traverse(name)
      tparams.foreach(x => traverse(x))
      traverse(ctor)
      traverse(templ)
    case Term.Tuple(elems) =>
      elems.foreach(x => traverse(x))
    case Ctor.Primary(mods, paramss) =>
      mods.foreach(x => traverse(x))
      paramss.foreach(params => params.foreach(x => traverse(x)))
    case Defn.Type(mods, name, tparams, body) =>
      mods.foreach(x => traverse(x))
      traverse(name)
      tparams.foreach(x => traverse(x))
      traverse(body)
    case Term.New(templ) =>
      traverse(templ)
    case _ : Mod.Case =>
    case Term.Throw(expr) =>
      traverse(expr)
    case _ : Mod.Override =>
    case Pat.ExtractInfix(lhs, ref, rhs) =>
      traverse(lhs)
      traverse(ref)
      rhs.foreach(x => traverse(x))
    case Type.Function(params, res) =>
      params.foreach(x => traverse(x))
      traverse(res)
    case Term.Function(params, body) =>
      params.foreach(x => traverse(x))
      traverse(body)
    case Type.Param(mods, name, tparams, contextBounds, viewBounds, typeBounds) =>
      mods.foreach(x => traverse(x))
      name.foreach(x => traverse(x))
      tparams.foreach(x => traverse(x))
      contextBounds.foreach(x => traverse(x))
      viewBounds.foreach(x => traverse(x))
      traverse(typeBounds)
    case _: Import.Wildcard =>
    case Import.Clause(ref, sels) =>
      traverse(ref)
      sels.foreach(x => traverse(x))
    case Defn.Object(mods, name, templ) =>
      mods.foreach(x => traverse(x))
      traverse(name)
      traverse(templ)
    case Pkg(ref, stats) =>
      traverse(ref)
      stats.foreach(x => traverse(x))
    case Term.ApplyType(fun, targs) =>
      traverse(fun)
      targs.foreach(x => traverse(x))
    case Source(stats) =>
      stats.foreach(x => traverse(x))
    case _: Mod.Abstract =>
    case Type.Bounds(lo, hi) =>
      lo.foreach(x => traverse(x))
      hi.foreach(x => traverse(x))
    case Import(clauses) =>
      clauses.foreach(x => traverse(x))
    case Pat.Tuple(elements) =>
      elements.foreach(x => traverse(x))
    case _ =>
  }
}