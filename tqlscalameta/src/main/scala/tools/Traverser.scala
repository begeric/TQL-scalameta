package tools
/**
 * Created by Eric on 25.11.2014.
 */

import scala.meta._
import org.scalameta.adt._

class Traverser extends TraverserBuilder[Tree]{
	def traverse(tree: Tree):Unit = traverseAdt
  def traverse2(tree: Tree):Unit = traverseAdt2
}