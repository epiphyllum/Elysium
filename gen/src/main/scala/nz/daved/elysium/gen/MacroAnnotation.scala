package nz.daved.elysium.gen

import scala.annotation.StaticAnnotation
import scala.meta._
import scala.collection.immutable._
import scala.meta.dialects.Paradise211
import nz.daved.elysium.manipulate.Implicits._

/**
  * The eventual idea here is to lift plain functions/methods into the context of a macro
  *
  * This does:
  *
  * - Check the input tree type.
  * - Check the return tree type
  * - Check all parameter types.
  * - Converts parameter literals to the literal type they are
  * - Makes parameter literals available in scope as the named parameter
  *
  * // TODO actually just call the other method, rather then manually inlining it
  * eg.
  *
  * @macroAnnotation
  * def identity(tree: meta.Tree): meta.Tree = tree
  *
  * becomes:
  *
  *
  * class identity extends StaticAnnotation {
  *   inline def apply(a: Any): Any = meta {
  *      a match {
  *        case tree: meta.Tree =>
  *         tree match {
  *            case t: meta.Tree =>
  *              t
  *            case _ =>
  *              abort("@identity must return a meta.Tree")
  *         }
  *        case _ =>
  *         abort("@identity only supports meta.Tree")
  *      }
  *   }
  * }
  *
  * and
  *
  * @macroAnnotation
  * def foo[T](bar: Defn.Val)(baz: String): Defn.Val = ???
  *
  * becomes:
  *
  * class foo[T](_baz: String) extends StaticAnnotation {
  *   inline def apply(a: Any): Any = meta {
  *      val baz = s match {
  *        case Lit(s: String) =>
  *          s
  *        case _ =>
  *          abort(expected 'baz' to be a string literal)
  *      }
  *
  *      a match {
  *        case bar: Defn.Val =>
  *         ???
  *        case _ =>
  *         abort("'Foo' only supports Defn.Val")
  *      }
  *   }
  * }
  */
class macroAnnotation extends StaticAnnotation {
  inline def apply(a: Any): Any = meta {
    val obj: Defn.Object = MacroAnnotation.asObject(a)
    val details = MacroAnnotationExtractor.unapply(obj).get
    val matcherAbortLit = Lit(s"expected input tree of type ${details.treeType.syntax}")
    val annotationLit = Lit(s"@${details.macroName} not expanded")
    val annotation = mod"@scala.annotation.compileTimeOnly($annotationLit)"
    val pat = Pat.Var.Term(Term.Name(details.treeName))
    val newStat = q"""$annotation inline def apply(a: Any): Any = meta {
            a match {
                case $pat: ${details.treeType} =>
                  ..${details.stats}
                case _ =>
                  abort($matcherAbortLit)
            }
         }"""

    val newStats: Seq[Stat] = newStat :: Nil
    abort(newStat.show[Structure])
    val className: Type.Name = Type.Name(obj.name.value)
    q"""object ${obj.name} { class $className extends scala.annotation.StaticAnnotation { ..$newStats } }"""
  }
}

case class MacroAnnotationDetails(
   stats: Seq[Stat],
   macroName: String,
   treeName: String,
   treeType: Type.Select)

object MacroAnnotationExtractor {
  def unapply(details: Defn.Object): Some[MacroAnnotationDetails] = {
    val applyMethod: Defn.Def = ApplyMethodExtractor.unnapply(details).get
    val stats: Seq[Stat] = StatExtractor.unapply(applyMethod.body).get
    val macroName: Term.Name = details.name
    val treeType: Type.Select = TypeSelectExtractor.unapply(applyMethod.paramss.head.head.decltpe.get).get
    val treeName: Term.Param.Name = applyMethod.paramss.head.head.name
    Some(MacroAnnotationDetails(stats, macroName.value, treeName.value, treeType))
  }
}

object TypeSelectExtractor {
  def unapply(arg: Type.Arg): Option[Type.Select] =
    arg match {
      case select: Type.Select =>
        Some(select)
      case _ =>
        abort("Could not extract select")
    }
}

object StatExtractor {
  def unapply(body: Term): Some[Seq[Stat]] = {
    body match {
      case block: Term.Block =>
        Some(block.stats)
      case other =>
        Some(other :: Nil)
      case _ =>
        abort("Could not extract stats")
    }
  }
}

object ApplyMethodExtractor {
  def unnapply(a: Defn.Object): Some[Defn.Def] = {
    a match {
      case Defn.Object(_, _, Template(_, _, _, Some((d @ Defn.Def(_)) :: Nil))) =>
        Some(d)
      case _ =>
        abort("Currently macroAnnotation must have an apply method")
    }
  }
}

object MacroAnnotation {

  // TODO: Tighten restrictions here
  // - Check for
  def asObject(a: Any): Defn.Object = {
    a match {
      case defn: Defn.Object =>
        defn
      case _ =>
        abort("Currently macroAnnotation only supports objects")
    }
  }
}