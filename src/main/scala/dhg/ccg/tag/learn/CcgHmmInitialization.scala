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

class CcgCombinabilityTrInitializer[Word](
  delegateInitializer: TransitionInitializer[Word, Cat],
  canCombine: CatCanCombine,
  combinableTransitionMass: Double = 0.95 // amount of probability mass reserved for combinable transitions
  ) extends TransitionInitializer[Word, Cat] {

  override def apply(
    sentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Word, Cat]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.toSet)
    val delegate = delegateInitializer(sentences, tagdict)
    new CcgCombinabilityTransitionConditionalProbabilityDistribution(delegate, tagdict, canCombine, combinableTransitionMass)
  }
  
  override def toString = f"CcgCombinabilityTrInitializer($delegateInitializer, $canCombine, $combinableTransitionMass)"
}

class CcgCombinabilityTransitionConditionalProbabilityDistribution[Word](
  delegate: ConditionalProbabilityDistribution[Cat, Cat],
  tagdict: TagDictionary[Word, Cat],
  canCombine: CatCanCombine,
  combinableTransitionMass: Double)
  extends ConditionalProbabilityDistribution[Cat, Cat] {

  def getCombinableSplitSums(t1: Cat) = {
    val (can, cant) =
      tagdict.allTagsSE.mapTo { t2 =>
        delegate(t2, t1)
      }.toMap.partition { case (t2, p) => canCombine(t1, t2) }
    (can.values.sum / combinableTransitionMass, cant.values.sum / (1 - combinableTransitionMass))
  }

  val combinableSplitSums = (tagdict.allTags + tagdict.startTag).mapTo(getCombinableSplitSums).toMap

  def apply(x: Cat, given: Cat): Double = {
    val (canZ, cantZ) = combinableSplitSums.getOrElse(given, getCombinableSplitSums(given))
    val z = if (canCombine(given, x)) canZ else cantZ
    delegate(x, given) / z
  }

  def sample(given: Cat): Cat = ???
}

//
//
//

class TagPriorTrInitializer[Word, Tag](
  tagPriorInitializer: TagPriorInitializer[Word, Tag])
  extends TransitionInitializer[Word, Tag] {
  override def apply(
    sentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Word, Tag]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.toSet)
    new SimpleConditionalProbabilityDistribution(Map(), Some(tagdict.allTagsSE), Some(tagdict.excludedTags + tagdict.endTag),
      tagPriorInitializer(sentences, tagdict))
  }

  override def toString = f"TagPriorTrInitializer($tagPriorInitializer)"
}
