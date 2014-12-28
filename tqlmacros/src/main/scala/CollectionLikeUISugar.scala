package tql

/**
 * Created by Eric on 28.12.2014.
 */

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
/**
 * This class, much like CombinatorsSugar, define several bundled macros used to enhance
 * or to make less boilerplaty the CollectionLikeUI
 * */
class CollectionLikeUISugar(override val c: Context) extends CombinatorsSugar(c) {
  import c.universe._

  /**
   * See comments in parent class (CombinatorsSugar).
   * This method has to be overriden because the 'prefix' has to be added in front of the
   * rewritten function.
   * */
  override def filterSugarImpl[T : c.WeakTypeTag](f: c.Tree): c.Tree = {
    //I know it's ugly but that's the only way I found to do it.
    val (lhs, _) =  getLUBsfromPFs[T](f)
    q"${c.prefix}.guard[$lhs]($f)"
  }

  /**
   * The implementation of transform is special in CollectionLikeUI because we have to make sure that several
   * transformations can be re-written.
   * */
  def transformSugarImplWithTRtype[T : c.WeakTypeTag](f: c.Tree): c.Tree =
    q"${c.prefix}.transforms(${transformSugarImpl[T](f)})"
}
