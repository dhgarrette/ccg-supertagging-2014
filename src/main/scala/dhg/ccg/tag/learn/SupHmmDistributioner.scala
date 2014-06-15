package dhg.ccg.tag.learn

import dhg.util.CollectionUtil._
import dhg.util.FileUtil._
import math.{ log, exp }
import com.typesafe.scalalogging.slf4j.{ StrictLogging => Logging }
import scalaz._
import Scalaz._
import dhg.util.Time
import scala.collection.breakOut
import dhg.ccg.prob._
import dhg.ccg.tag.TagDictionary

trait TransitionDistributioner[Word, Tag] {
  def apply(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Word, Tag]): ConditionalProbabilityDistribution[Tag, Tag]

  def make(
    transitionCounts: Map[Tag, Map[Tag, Double]],
    tagCounts: Map[Tag, Double], // INCLUDING START/END
    initialTagdict: TagDictionary[Word, Tag]): ConditionalProbabilityDistribution[Tag, Tag]
}

abstract class AbstractTransitionDistributioner[Word, Tag] extends TransitionDistributioner[Word, Tag] {
  def apply(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Word, Tag]): ConditionalProbabilityDistribution[Tag, Tag] = {
    val tagdict = initialTagdict.withWords(taggedSentences.flatten.map(_._1).toSet).withTags(taggedSentences.flatten.map(_._2).toSet)
    val transitionCounts =
      taggedSentences
        .flatMap(s => (tagdict.startTag +: s.map(_._2) :+ tagdict.endTag).sliding2)
        .filter { case (at, bt) => !tagdict.excludedTags(at) && !tagdict.excludedTags(bt) }
        .groupByKey.mapVals(_.counts.mapVals(_.toDouble))
    val tagCounts = taggedSentences.flatMap(s => (tagdict.startTag +: s.map(_._2) :+ tagdict.endTag)).counts.mapVals(_.toDouble) -- tagdict.excludedTags
    make(transitionCounts, tagCounts, tagdict)
  }
}

class UnsmoothedTransitionDistributioner[Word, Tag]()
  extends AbstractTransitionDistributioner[Word, Tag] {
  def make(
    transitionCounts: Map[Tag, Map[Tag, Double]],
    tagCounts: Map[Tag, Double], // INCLUDING START/END
    tagdict: TagDictionary[Word, Tag]) = {
    new SimpleConditionalProbabilityDistribution((transitionCounts -- tagdict.excludedTags).mapVals(c => new SimpleProbabilityDistribution(c -- tagdict.excludedTags)),
      None,
      Some(tagdict.excludedTags + tagdict.endTag),
      ProbabilityDistribution.empty)
  }
  override def toString = f"UnsmoothedTransitionDistributioner()"
}

class AddLambdaTransitionDistributioner[Word, Tag](lambda: Double = 0.2)
  extends AbstractTransitionDistributioner[Word, Tag] {
  def make(
    transitionCounts: Map[Tag, Map[Tag, Double]],
    tagCounts: Map[Tag, Double], // INCLUDING START/END
    tagdict: TagDictionary[Word, Tag]) = {
    val allTransitionToAbleTags = Some(tagdict.allTags + tagdict.endTag)
    val startTag = Some(Set(tagdict.startTag) ++ tagdict.excludedTags)
    new SimpleConditionalProbabilityDistribution(
      (tagdict.allTags.mapToVal(Map[Tag, Double]()).toMap ++ transitionCounts -- tagdict.excludedTags)
        .mapVals(new LaplaceProbabilityDistribution(_, allTransitionToAbleTags, startTag, lambda)) +
        (tagdict.startTag -> new LaplaceProbabilityDistribution(transitionCounts.getOrElse(tagdict.startTag, Map.empty), allTransitionToAbleTags, Some(Set(tagdict.startTag, tagdict.endTag) ++ tagdict.excludedTags), lambda)) + // Start Tag can't transition to End Tag
        (tagdict.endTag -> ProbabilityDistribution.empty), // End Tag can't transition to anything
      None,
      Some(tagdict.excludedTags + tagdict.endTag),
      new LaplaceProbabilityDistribution(Map(), allTransitionToAbleTags, startTag, lambda))
  }
  override def toString = f"AddLambdaTransitionDistributioner($lambda)"
}

//
//
//

trait EmissionDistributioner[Word, Tag] {
  def apply(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Word, Tag]): ConditionalProbabilityDistribution[Tag, Word]

  def make(
    emissionCounts: Map[Tag, Map[Word, Double]],
    initialTagdict: TagDictionary[Word, Tag]): ConditionalProbabilityDistribution[Tag, Word]
}

abstract class AbstractEmissionDistributioner[Word, Tag] extends EmissionDistributioner[Word, Tag] {
  def apply(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Word, Tag]): ConditionalProbabilityDistribution[Tag, Word] = {
    val tagdict = initialTagdict.withWords(taggedSentences.flatten.map(_._1).to[Set]).withTags(taggedSentences.flatten.map(_._2).to[Set])
    val emissionCounts = taggedSentences.flatten.filter { case (w, t) => tagdict(w)(t) && !tagdict.excludedTags(t) }.map(_.swap).groupByKey.mapVals(_.counts.mapVals(_.toDouble))
    make(emissionCounts, tagdict)
  }
}

class UnsmoothedEmissionDistributioner[Word, Tag]()
  extends AbstractEmissionDistributioner[Word, Tag] {
  def make(
    emissionCounts: Map[Tag, Map[Word, Double]],
    initialTagdict: TagDictionary[Word, Tag]): ConditionalProbabilityDistribution[Tag, Word] = {

    val tagdict = initialTagdict.withWords(emissionCounts.values.flatMap(_.keys).to[Set]).withTags(emissionCounts.keySet)
    val allWordsSet = Some(tagdict.allWords)
    new SimpleConditionalProbabilityDistribution(
      (emissionCounts -- tagdict.excludedTags).mapt((t, tcounts) => t -> new LaplaceProbabilityDistribution(tcounts, allWordsSet, Some(tagdict.entries.keySet -- tagdict.knownWordsForTag(t) + tagdict.startWord + tagdict.endWord), 0.0)) +
        (tagdict.startTag -> new SimpleProbabilityDistribution(Map(tagdict.startWord -> 1.0))) +
        (tagdict.endTag -> new SimpleProbabilityDistribution(Map(tagdict.endWord -> 1.0))),
      None,
      Some(tagdict.excludedTags),
      ProbabilityDistribution.empty)
  }
  override def toString = f"UnsmoothedEmissionDistributioner()"
}

class AddLambdaEmissionDistributioner[Word, Tag](lambda: Double = 0.1)
  extends AbstractEmissionDistributioner[Word, Tag] {
  def make(
    emissionCounts: Map[Tag, Map[Word, Double]],
    initialTagdict: TagDictionary[Word, Tag]): ConditionalProbabilityDistribution[Tag, Word] = {

    val tagdict = initialTagdict.withWords(emissionCounts.values.flatMap(_.keys).to[Set]).withTags(emissionCounts.keySet)
    val allWordsSet = Some(tagdict.allWords)
    new SimpleConditionalProbabilityDistribution(
      (tagdict.allTags.mapToVal(Map[Word, Double]()).toMap ++ emissionCounts -- tagdict.excludedTags)
        .mapt((t, tcounts) => t -> new LaplaceProbabilityDistribution(tcounts, allWordsSet, Some(tagdict.entries.keySet -- tagdict.knownWordsForTag.getOrElse(t, Set.empty) + tagdict.startWord + tagdict.endWord), lambda)) +
        (tagdict.startTag -> new SimpleProbabilityDistribution(Map(tagdict.startWord -> 1.0))) +
        (tagdict.endTag -> new SimpleProbabilityDistribution(Map(tagdict.endWord -> 1.0))),
      None,
      Some(tagdict.excludedTags),
      new LaplaceProbabilityDistribution(Map(), allWordsSet, Some(tagdict.entries.keySet + tagdict.startWord + tagdict.endWord), 1.0))
  }
  override def toString = f"AddLambdaEmissionDistributioner($lambda)"
}
