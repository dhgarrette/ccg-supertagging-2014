package dhg.ccg.tag.learn

import dhg.util.CollectionUtil._
import dhg.util.Time._
import dhg.util.FileUtil._
import dhg.util.StringUtil._
import scalaz._
import scalaz.Scalaz._
import dhg.ccg.prob._
import dhg.ccg.tag.TagDictionary
import com.typesafe.scalalogging.slf4j.{ StrictLogging => Logging }

trait TransitionInitializer[Word, Tag] extends (( //
Vector[Vector[Word]], // sentences
TagDictionary[Word, Tag] // tagdict
) => ConditionalProbabilityDistribution[Tag, Tag]) {
  def apply(
    sentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Word, Tag] //
    ): ConditionalProbabilityDistribution[Tag, Tag]
}

class TrUniform[Word, Tag]() extends TransitionInitializer[Word, Tag] {
  override def apply(
    sentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Word, Tag]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.toSet)
    val allTags = Some(tagdict.allTags)
    val startTag = Some(tagdict.excludedTags + tagdict.startTag)
    new SimpleConditionalProbabilityDistribution(
      Map(
        tagdict.startTag -> new LaplaceProbabilityDistribution(Map(), allTags, Some(tagdict.excludedTags + tagdict.startTag + tagdict.endTag), 1.0),
        tagdict.endTag -> ProbabilityDistribution.empty),
      None,
      Some(tagdict.excludedTags + tagdict.endTag),
      new LaplaceProbabilityDistribution(Map(), Some(tagdict.allTags + tagdict.endTag), startTag, 1.0))
  }
  override def toString = f"TrUniform()"
}

class TrTagDictEntriesPossibilities[Word, Tag](distributioner: TransitionDistributioner[Word, Tag]) extends TransitionInitializer[Word, Tag] with Logging {
  override def apply(
    sentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Word, Tag]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.toSet)
    logger.info("TrTagDictEntriesPossibilities => sentenceSets")
    val tdEntries = tagdict.entries
    val sentenceSets = sentences.map(_.map(w => tdEntries.getOrElse(w, Set())))
    logger.info("TrTagDictEntriesPossibilities => trCounts")
    val potentialTransitions =
      for {
        s <- sentenceSets
        (as, bs) <- (Set(tagdict.startTag) +: s :+ Set(tagdict.endTag)).sliding2
        c = 1.0 / (as.size * bs.size)
        a <- as; b <- bs
      } yield {
        (a, (b, c))
      }
    val trCounts = potentialTransitions.groupByKey.mapVals(_.groupByKey.mapVals(_.sum))
    logger.info("TrTagDictEntriesPossibilities => tagCounts")
    val tagCounts = sentenceSets.flatten.flatMap(ts => ts.mapToVal(1.0 / ts.size)).groupByKey.mapVals(_.sum)
    logger.info("TrTagDictEntriesPossibilities => make distribution")
    distributioner.make(trCounts, tagCounts, tagdict)
  }
  override def toString = f"TrTagDictEntriesPossibilities($distributioner)"
}

/**
 *
 */
class InterpolatingTransitionInitializer[Word, Tag](
  delegates: Vector[(TransitionInitializer[Word, Tag], Double)])
  extends TransitionInitializer[Word, Tag] {

  def apply(
    sentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Word, Tag] //
    ): ConditionalProbabilityDistribution[Tag, Tag] = {
    new InterpolatingConditionalProbabilityDistribution(
      delegates.mapKeys(i => i(sentences, initialTagdict)))
  }

  def sample(given: Tag): Tag = ???
  override def toString = f"InterpolatingInitializer(${delegates.map { case (i, w) => f"$i -> $w" }.mkString(", ")})"
}

//
//
//

trait EmissionInitializer[Word, Tag] extends (( //
Vector[Vector[Word]], // sentences
TagDictionary[Word, Tag] // tagdict
) => ConditionalProbabilityDistribution[Tag, Word]) {
  def apply(
    sentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Word, Tag] //
    ): ConditionalProbabilityDistribution[Tag, Word]
}

class EmUniform[Word, Tag]() extends EmissionInitializer[Word, Tag] {
  override def apply(
    sentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Word, Tag]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.toSet)
    val knownWordsForTag = tagdict.entries.ungroup.map(_.swap).groupByKey.mapVals(_.toSet).withDefaultValue(Set.empty)
    val knownWords = knownWordsForTag.flatMap(_._2).toSet
    val allWordsSet = Some(tagdict.allWords)
    new SimpleConditionalProbabilityDistribution(
      tagdict.allTags.mapTo(t => new LaplaceProbabilityDistribution(Map(), allWordsSet, Some(knownWords -- knownWordsForTag(t) + tagdict.startWord + tagdict.endWord), 1.0)).toMap +
        (tagdict.startTag -> new SimpleProbabilityDistribution(Map(tagdict.startWord -> 1.0))) +
        (tagdict.endTag -> new SimpleProbabilityDistribution(Map(tagdict.endWord -> 1.0))),
      None,
      Some(tagdict.excludedTags),
      new LaplaceProbabilityDistribution(Map(), allWordsSet, Some(knownWords + tagdict.startWord + tagdict.endWord), 1.0))
  }
  override def toString = f"EmUniform()"
}

class TagDictEstimateTagPriorInitializer[Word, Tag](tdCountLambda: Double = 0.26) extends TagPriorInitializer[Word, Tag] {
  def apply(sentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Word, Tag]): ProbabilityDistribution[Tag] = {
    val tagdict = initialTagdict.withWords(sentences.flatten.toSet)

    // TODO: BEGIN DUPLICATED
    val C = sentences.flatten.counts // C(w)
    val C_k = tagdict.entries.mapt { (w, ts) =>
      val partialCounts = (C.getOrElse(w, 0) + tdCountLambda) / ts.size.toDouble
      ts.mapTo(t => w -> partialCounts)
    }.toVector.flatten.groupByKey.mapVals(_.groupByKey.map { case (w, Vector(c)) => w -> c })
    // TODO: END DUPLICATED

    // p(t) = sum_w' C_k(t,w') / Z
    new LaplaceProbabilityDistribution(tagdict.allTags.mapTo(C_k(_).values.sum).toMap, None, Some(tagdict.excludedTags), 0.0)
  }
}

class EmTagDictEstimate[Word, Tag](tagPriorInit: TagPriorInitializer[Word, Tag], lambda: Double = 0.04, tdCountLambda: Double = 0.26, combineKU: Boolean = false) extends EmissionInitializer[Word, Tag] {
  override def apply(
    sentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Word, Tag]) = {

    /* We normally smooth emissions from C(t,w)
     *   p(w|t) = C(t,w) / C(t)
     *   
     * C(t,w) comes in two varieties:
     * 
     *   1. if w is in the TD: C_k(t,w)
     *      - if t in TD(w): C_k(t,w) = C(w) / |TD(w)|
     *        else:          C_k(t,w) = 0
     *   
     *   2. if w is not in the TD: C_u(t,w)      (td-unknown)
     *      - C_u(t,w) = C(w) * p(t|unk)         (divide the counts of w among all tags according 
     *                                            to likelihood of t given that w is unknown)
     *      - p(t|unk) = p(unk|t) * p(t) / Z     (Bayes rule)
     *      - p(t) = sum_w' C_k(t,w') / Z
     *      - p(unk|t) = C(t,unk) / Z XX
     *                 = |TD(t)| / (sum_t' |TD(t')| for known words only)
     *                 
     * p(w|t) = (C_k(t,w) + C_u(t,w)) / Z
     *   
     */

    val sentenceWords = sentences.flatten.toSet
    val tagdict = initialTagdict.withWords(sentences.flatten.toSet)

    val knownWordsForTag = tagdict.knownWordsForTag
    val knownWords = knownWordsForTag.flatMap(_._2).toSet

    val C = sentences.flatten.counts // C(w)

    // C_k(t)(w)
    val C_k = tagdict.allTags.mapToVal(Map.empty[Word,Double]).toMap ++ tagdict.entries.mapt { (w, ts) =>
      val partialCounts = (C.getOrElse(w, 0) + tdCountLambda) / ts.size.toDouble
      ts.mapTo(t => w -> partialCounts)
    }.toVector.flatten.groupByKey.mapVals(_.groupByKey.map { case (w, Vector(c)) => w -> c })

    val td_unknown_words = sentenceWords -- knownWords
    // p(t)
    val p = tagPriorInit(sentences, tagdict)
    // p(unk|t) = |TD(t)| / (sum_t' |TD(t')| for known words only)
    val `p(unk|t)` = tagdict.allTags.mapTo(knownWordsForTag(_).size + 0.001).toMap.normalizeValues
    // p(t|unk) = p(unk|t) * p(t) / Z
    val `p(t|unk)` = tagdict.allTags.mapTo(t => `p(unk|t)`(t) * p(t)).toMap.normalizeValues
    // C_u(t)(w) = C(w) * p(t|unk)
    val C_u =
      tagdict.allTags.mapTo { t =>
        td_unknown_words.mapTo { w =>
          C(w) * `p(t|unk)`(t)
        }.toMap
      }.toMap

    val C_ku = tagdict.allTags.mapTo { t =>
      val cut = C_u(t)
      val ckt = C_k(t)
      if (combineKU)
        cut |+| ckt
      else
        cut ++ ckt
    }.toMap

    val allWordsSet = Some(tagdict.allWords)
    new SimpleConditionalProbabilityDistribution(
      C_ku.mapt((t, counts) => t -> new LaplaceProbabilityDistribution(counts, allWordsSet, Some(knownWords -- knownWordsForTag(t) + tagdict.startWord + tagdict.endWord), lambda)).toMap +
        (tagdict.startTag -> new SimpleProbabilityDistribution(Map(tagdict.startWord -> 1.0))) +
        (tagdict.endTag -> new SimpleProbabilityDistribution(Map(tagdict.endWord -> 1.0))),
      None,
      Some(tagdict.excludedTags),
      new LaplaceProbabilityDistribution(C_ku.values.reduce(_ |+| _), allWordsSet, Some(knownWords + tagdict.startWord + tagdict.endWord), lambda))
  }
  override def toString = f"EmTagDictEstimate($lambda, $tdCountLambda, $combineKU)"
}
