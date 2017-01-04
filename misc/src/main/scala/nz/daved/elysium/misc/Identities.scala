package nz.daved.elysium.misc

import nz.daved.elysium.gen.macroAnnotation

import scala.meta._

@macroAnnotation
object identity {
  def apply(tree: Tree): Tree = tree
}

@macroAnnotation
object copyDef {
  def apply(defn: Defn.Def): Defn.Def = defn.copy()
}

