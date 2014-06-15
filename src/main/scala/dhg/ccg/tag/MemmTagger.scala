package dhg.ccg.tag

import java.io.BufferedOutputStream
import java.io.FileInputStream
import java.io.FileOutputStream
import dhg.util.Arm.ManagedCloseable
import dhg.util.Arm.using
import dhg.util.CollectionUtil._
import opennlp.tools.dictionary.Dictionary
import opennlp.tools.postag.POSDictionary
import opennlp.tools.postag.POSModel
import opennlp.tools.postag.POSSample
import opennlp.tools.postag.POSTaggerME
import opennlp.tools.util.ObjectStreamUtils
import opennlp.tools.util.TrainingParameters
import opennlp.tools.util.model.ModelType
import opennlp.tools.postag.OpenNlpPosDictionary._
import dhg.util.math.LogDouble

/**
 * Train a MEMM from gold-labeled data.
 *
 * @param maxIterations
 * @param cutoff			"events" must occur at least this many times to be used during training
 */
class MemmTaggerTrainer[Word, Tag](
  maxIterations: Int = 50,
  cutoff: Int = 100,
  tdRestricted: Boolean = false,
  wordToString: (Word => String), wordFromString: (String => Word),
  tagToString: (Tag => String), tagFromString: (String => Tag))
  extends SupervisedTaggerTrainer[Word, Tag] {

  override def train(taggedSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Word, Tag]) = {
    val (taggedSentencesWords, taggedSentencesTags) = taggedSentences.flatten.toSet.unzip
    val tagdict = initialTagdict.withWords(taggedSentencesWords).withTags(taggedSentencesTags)

    val samples = ObjectStreamUtils.createObjectStream(
      taggedSentences.map { s =>
        val (words, tags) = s.unzip
        new POSSample(words.map(wordToString).toArray, tags.map(tagToString).toArray)
      }: _*)

    val languageCode = "Uh... any language ???"

    val params = new TrainingParameters()
    params.put(TrainingParameters.ALGORITHM_PARAM, ModelType.MAXENT.toString)
    params.put(TrainingParameters.ITERATIONS_PARAM, maxIterations.toString)
    params.put(TrainingParameters.CUTOFF_PARAM, cutoff.toString)

    val tagDictionary =
      if (tdRestricted) {
        taggedSentencesWords.foldLeft(new POSDictionary) { (td, w) =>
          td.updated(wordToString(w), (tagdict(w) & taggedSentencesTags).map(tagToString))
        }
      }
      else null

    val ngramDictionary: Dictionary = null

    val model = POSTaggerME.train(
      languageCode,
      samples,
      params,
      tagDictionary,
      ngramDictionary)
    new MemmTagger[Word, Tag](new POSTaggerME(model), wordToString, wordFromString, tagToString, tagFromString)
  }
}

object MemmTaggerTrainer {
  def apply[Word, Tag](
    maxIterations: Int = 50,
    cutoff: Int = 100,
    tdRestricted: Boolean = false,
    wordToString: (Word => String), wordFromString: (String => Word),
    tagToString: (Tag => String), tagFromString: (String => Tag)) =
    new MemmTaggerTrainer(maxIterations, cutoff, tdRestricted, wordToString, wordFromString, tagToString, tagFromString)

  def apply2[Tag](
    maxIterations: Int = 50,
    cutoff: Int = 100,
    tdRestricted: Boolean = false,
    tagToString: (Tag => String), tagFromString: (String => Tag)) =
    new MemmTaggerTrainer[String, Tag](maxIterations, cutoff, tdRestricted, identity, identity, tagToString, tagFromString)

  def apply1(
    maxIterations: Int = 50,
    cutoff: Int = 100,
    tdRestricted: Boolean = false) =
    new MemmTaggerTrainer[String, String](maxIterations, cutoff, tdRestricted, identity, identity, identity, identity)
}

class TypeShiftingTagDictionary[W1, T1, Word, Tag](
  delegate: TagDictionary[W1, T1],
  wordToString: (W1 => Word), wordFromString: (Word => W1),
  tagToString: (T1 => Tag), tagFromString: (Tag => T1))
  extends TagDictionary[Word, Tag] {

  def allWords: Set[Word] = delegate.allWords.map(wordToString)
  def allTags: Set[Tag] = delegate.allTags.map(tagToString)

  def startWord: Word = wordToString(delegate.startWord)
  def startTag: Tag = tagToString(delegate.startTag)
  def endWord: Word = wordToString(delegate.endWord)
  def endTag: Tag = tagToString(delegate.endTag)
  
  def excludedTags: Set[Tag] = delegate.excludedTags.map(tagToString)

  def apply(w: Word): Set[Tag] = delegate(wordFromString(w)).map(tagToString)

  def reversed: Map[Tag, Set[Word]] = ???

  def entries: Map[Word, Set[Tag]] = delegate.entries.mapt((w, ts) => wordToString(w) -> ts.map(tagToString))
  def knownWordsForTag: Map[Tag, Set[Word]] = delegate.knownWordsForTag.mapt((t, ws) => tagToString(t) -> ws.map(wordToString))

  def withWords(words: Set[Word]): TagDictionary[Word, Tag] = ???
  def withTags(tags: Set[Tag]): TagDictionary[Word, Tag] = ???
  def withExcludedTags(tags: Set[Tag]): TagDictionary[Word, Tag] = ???
}

//
//
//

class MemmTagger[Word, Tag](
  meTagger: POSTaggerME,
  wordToString: (Word => String), wordFromString: (String => Word),
  tagToString: (Tag => String), tagFromString: (String => Tag)) extends Tagger[Word, Tag] {

  def sentenceProb(sentence: Vector[(Word, Tag)]): LogDouble = ???

  def tagAndProb(sentence: Vector[Word]): (Vector[Tag], LogDouble) = {
    val arr = sentence.map(wordToString).toArray
    val taggedArray = meTagger.tag(arr)
    val taggedVector = taggedArray.toVector
    val converted = taggedVector.map(tagFromString)
    (converted, new LogDouble(Double.PositiveInfinity))
  }
}
