package dhg.ccg.tag.learn

import dhg.util.CollectionUtil._
import dhg.util.Time._
import dhg.util.FileUtil._
import dhg.util.StringUtil._
import math.pow
import scalaz.{ \/ => _, _ }
import scalaz.Scalaz._
import dhg.ccg.cat._
import dhg.ccg.prob._
import dhg.ccg.tag.TagDictionary

trait TagPriorInitializer[Word, Tag] {
  def apply(sentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Word, Tag]): ProbabilityDistribution[Tag]
}

class UniformTagPriorInitializer[Word, Tag] extends TagPriorInitializer[Word, Tag] {
  def apply(sentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Word, Tag]): ProbabilityDistribution[Tag] = {
    new LaplaceProbabilityDistribution(Map(), Some(initialTagdict.allTags), None, 1.0, totalAddition = 100.0)
  }
  override def toString = f"UniformTagPriorInitializer()"
}

class CatComplexityInitializer[Word] extends TagPriorInitializer[Word, Cat] {
  override def apply(sentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Word, Cat]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.toSet)
    new CatComplexityProbabilityDistribution(tagdict)
  }

  override def toString = f"CatComplexityInitializer()"
}

/**
 * Baldridge (2008)
 */
class CatComplexityProbabilityDistribution[Word](
  tagdict: TagDictionary[Word, Cat])
  extends ProbabilityDistribution[Cat] {

  private[this] val z = tagdict.allTagsSE.sumBy(1.0 / _.complexity)

  def apply(c: Cat): Double = if (tagdict.excludedTags(c)) 0.0 else (1.0 / c.complexity) / z
  def sample(): Cat = ???
  def defaultProb: Double = 0.0
}

//

/**
 * For use when pAtom is given
 */
class SimpleCatgramCatPriorInitializer[Word](
  pAtom: ProbabilityDistribution[AtomCat],
  pTerm: Double, // must be > 0.5 to ensure finiteness
  pMod: Double, //  probability mass devoted to modifier categories
  pFwd: Double)
  extends TagPriorInitializer[Word, Cat] {
  def apply(sentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Word, Cat]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.toSet)
    new CatPriorProbabilityDistribution(tagdict, pAtom, pTerm, pMod, pFwd)
  }

  override def toString = f"SimpleCatgramCatPriorInitializer($pAtom, pTerm=$pTerm%.3f, pMod=$pMod%.3f, pFwd=$pFwd%.3f)"
}

/**
 * Assume uniform pAtom
 */
class UniformCatgramAtomCatPriorInitializer[Word](
  pTerm: Double, // must be > 0.5 to ensure finiteness
  pMod: Double, //  probability mass devoted to modifier categories
  pFwd: Double)
  extends TagPriorInitializer[Word, Cat] {
  override def apply(sentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Word, Cat]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.toSet)
    def atoms(c: Cat): Set[AtomCat] = c match { case a: AtomCat => Set(a); case l \/ r => atoms(l) | atoms(r) }
    val pAtom = new LaplaceProbabilityDistribution[AtomCat](Map(), Some(tagdict.allTagsSE.flatMap(atoms)), Some(tagdict.excludedTags), 1.0)
    new CatPriorProbabilityDistribution(tagdict, pAtom, pTerm, pMod, pFwd)
  }

  override def toString = f"UniformCatgramAtomCatPriorInitializer(pTerm=$pTerm%.3f, pMod=$pMod%.3f, pFwd=$pFwd%.3f)"
}

/**
 * Set pAtom using tagdict+raw statistics
 */
class TagdictInformedCatgramCatPriorInitializer[Word](
  pTerm: Double, // must be > 0.5 to ensure finiteness
  pMod: Double, //  probability mass devoted to modifier categories
  pFwd: Double,
  atomLambda: Double)
  extends TagPriorInitializer[Word, Cat] {
  override def apply(sentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Word, Cat]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.toSet)

    def atomCounts(c: Cat): Map[AtomCat, Int] = c match { case a: AtomCat => Map(a -> 1); case l \/ r => atomCounts(l) |+| atomCounts(r) }

    val C = sentences.flatten.counts // C(w)
    val C_k = tagdict.entries.toVector.flatMap {
      case (w, ts) =>
        val partialCounts = (C.getOrElse(w, 0) + 1.0) / ts.size
        ts.mapTo(t => partialCounts)
    }.groupByKey.mapVals(_.sum) +
      (tagdict.endTag -> sentences.size.toDouble)

    val estAtomCounts = C_k.toVector.flatMap { case (t, c) => atomCounts(t).mapVals(_ * c) }.groupByKey.mapVals(_.sum)

    val pAtom = new LaplaceProbabilityDistribution[AtomCat](estAtomCounts, Some(tagdict.allTagsSE.flatMap(t => atomCounts(t).keys)), Some(tagdict.excludedTags), atomLambda)

    new CatPriorProbabilityDistribution(tagdict, pAtom, pTerm, pMod, pFwd)
  }

  override def toString = f"TagdictInformedCatgramCatPriorInitializer(pTerm=$pTerm%.3f, pMod=$pMod%.3f, pFwd=$pFwd%.3f, atomLambda=$atomLambda%.3f)"
}

//

class CatPriorProbabilityDistribution[Word](
  tagdict: TagDictionary[Word, Cat],
  pAtom: ProbabilityDistribution[AtomCat],
  pTerm: Double,
  pMod: Double,
  pFwd: Double)
  extends ProbabilityDistribution[Cat] {

  import CatPriorProbabilityDistribution._

  private[this] val z = tagdict.allTagsSE.sumBy(p)

  def apply(c: Cat): Double = p(c) / z

  private[this] def p(c: Cat): Double = {
    c match {
      case _ if tagdict.excludedTags(c) => 0.0
      case atom: AtomCat => pTerm * pAtom(atom)
      case FMod(x) => ~pTerm * pMod * pFwd * p(x)
      case l / r => ~pTerm * ~pMod * pFwd * p(l) * p(r)
      case BMod(x) => ~pTerm * pMod * ~pFwd * p(x)
      case l \ r => ~pTerm * ~pMod * ~pFwd * p(l) * p(r)
    }
  }

  def sample(): Cat = ???
  def defaultProb: Double = 0.0
}

object CatPriorProbabilityDistribution {
  implicit class DoubleProb(val v: Double) extends AnyVal { def unary_~ = 1 - v }
  object FMod { def unapply(c: Cat) = c match { case l / r if l == r => Some(l); case _ => None } }
  object BMod { def unapply(c: Cat) = c match { case l \ r if l == r => Some(l); case _ => None } }
}
