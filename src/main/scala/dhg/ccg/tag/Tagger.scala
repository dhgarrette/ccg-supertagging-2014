package dhg.ccg.tag

import dhg.util.CollectionUtil._
import dhg.ccg.prob.ConditionalProbabilityDistribution
import dhg.util.math.LogDouble

trait Tagger[Word, Tag] {

  /**
   * Compute the probability of the tagged sentence.
   */
  def sentenceProb(sentence: Vector[(Word, Tag)]): LogDouble

  /**
   * Accepts a sentence of word tokens and returns a sequence of
   * tags corresponding to each of those words.
   */
  final def tag(sentence: Vector[Word]): Vector[Tag] = tagAndProb(sentence)._1

  /**
   * Accepts a sentence of word tokens and returns a sequence of
   * tags corresponding to each of those words, along with the
   * probability of the tagged sentence.
   */
  def tagAndProb(sentence: Vector[Word]): (Vector[Tag], LogDouble)

}

trait WeightedTagger[Word, Tag] extends Tagger[Word, Tag] {
  override def sentenceProb(sentence: Vector[(Word, Tag)]): LogDouble = sentenceProbWithWeights(sentence, us = Vector.fill(sentence.size)(Map.empty))
  override def tagAndProb(sentence: Vector[Word]): (Vector[Tag], LogDouble) = tagAndProbWithWeights(sentence, us = Vector.fill(sentence.size)(Map.empty))

  def sentenceProbWithWeights(sentence: Vector[(Word, Tag)], us: Vector[Map[Tag, LogDouble]]): LogDouble
  final def tagWithWeights(sentence: Vector[Word], us: Vector[Map[Tag, LogDouble]]): Vector[Tag] = tagAndProbWithWeights(sentence, us)._1
  def tagAndProbWithWeights(sentence: Vector[Word], us: Vector[Map[Tag, LogDouble]]): (Vector[Tag], LogDouble)
  def tagAndProbWithWeightsFromTagSet(sentence: Vector[(Word, Set[Tag])], us: Vector[Map[Tag, LogDouble]]): (Vector[Tag], LogDouble)
}

//
//
//

trait SupervisedTaggerTrainer[Word, Tag] {
  def train(taggedSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Word, Tag]): Tagger[Word, Tag]
}

trait SemisupervisedTaggerTrainer[Word, Tag] {
  final def train(
    rawSentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Word, Tag],
    baseTrDist: ConditionalProbabilityDistribution[Tag, Tag], baseEmDist: ConditionalProbabilityDistribution[Tag, Word]): Tagger[Word, Tag] = {
    trainWithSomeGold(rawSentences, Vector.empty, initialTagdict, baseTrDist, baseEmDist)
  }

  final def trainWithTagsets(
    rawSentencesWithTokenTags: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Word, Tag],
    transitions: ConditionalProbabilityDistribution[Tag, Tag], emissions: ConditionalProbabilityDistribution[Tag, Word]): Tagger[Word, Tag] = {
    trainWithTagsetsAndSomeGold(rawSentencesWithTokenTags, Vector.empty, initialTagdict, transitions, emissions)
  }

  final def trainWithSomeGold(
    rawSentences: Vector[Vector[Word]], goldLabeledSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Word, Tag],
    transitions: ConditionalProbabilityDistribution[Tag, Tag], emissions: ConditionalProbabilityDistribution[Tag, Word]) = {
    trainWithTagsetsAndSomeGold(rawSentences.map(_.mapTo(initialTagdict)), goldLabeledSentences, initialTagdict, transitions, emissions)
  }

  def trainWithTagsetsAndSomeGold(
    rawSentencesWithTokenTags: Vector[Vector[(Word, Set[Tag])]], goldLabeledSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Word, Tag],
    transitions: ConditionalProbabilityDistribution[Tag, Tag], emissions: ConditionalProbabilityDistribution[Tag, Word]): Tagger[Word, Tag]
}

trait TypeSupervisedTaggerTrainer[Word, Tag] {
  def typesupTrain(rawSentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Word, Tag]): Tagger[Word, Tag]
}

trait NoisilySupervisedTaggerTrainer[Word, Tag] {
  final def noisySupTrain(noisilyLabeledSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Word, Tag]): Tagger[Word, Tag] = {
    noisySupTrainWithSomeGold(noisilyLabeledSentences, Vector.empty, initialTagdict)
  }
  def noisySupTrainWithSomeGold(noisilyLabeledSentences: Vector[Vector[(Word, Tag)]], goldLabeledSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Word, Tag]): Tagger[Word, Tag]
}
