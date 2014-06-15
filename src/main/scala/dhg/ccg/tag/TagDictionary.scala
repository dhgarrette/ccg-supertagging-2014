package dhg.ccg.tag

import dhg.util.CollectionUtil._
import dhg.util.FileUtil._
import dhg.util.StringUtil._
import scalaz._
import scalaz.Scalaz._

trait TagDictionary[Word, Tag] extends (Word => Set[Tag]) {
  def allWords: Set[Word]; def allTags: Set[Tag]
  def startWord: Word; def startTag: Tag; def endWord: Word; def endTag: Tag
  def excludedTags: Set[Tag]

  def apply(w: Word): Set[Tag]

  final def allWordsSE = allWords + (startWord, endWord)
  final def allTagsSE = allTags + (startTag, endTag)

  def reversed: Map[Tag, Set[Word]]

  def entries: Map[Word, Set[Tag]]
  def knownWordsForTag: Map[Tag, Set[Word]]

  def withWords(words: Set[Word]): TagDictionary[Word, Tag]
  def withTags(tags: Set[Tag]): TagDictionary[Word, Tag]
  def withExcludedTags(tags: Set[Tag]): TagDictionary[Word, Tag]
}

/**
 * ONLY INSTANTIATE THIS VIA THE COMPANION OBJECT
 *
 * A Tag Dictionary is a mapping from words to all of their potential
 * tags. A word not found in the dictionary (including "unknown" words)
 * may take any tag.
 *
 * This class guarantees that looking up the startWord or endWord will
 * return a set containing ony the startTag or endTag, respectively.
 *
 * The allWords property is the complete set of known words excluding
 * the special startWord and endWord.  Likewise for allTags.  For the
 * complete set of known words and tags including these special tags
 * use allWordsSE and allTagsSE.
 */
class SimpleTagDictionary[Word, Tag] private (
  map: Map[Word, Set[Tag]],
  val allWords: Set[Word], val allTags: Set[Tag],
  val startWord: Word, val startTag: Tag, val endWord: Word, val endTag: Tag,
  val excludedTags: Set[Tag] = Set.empty)
  extends TagDictionary[Word, Tag] {

  def apply(w: Word): Set[Tag] = {
    map.get(w).map(_ -- excludedTags).filter(_.nonEmpty).getOrElse(allTags) -- excludedTags
  }

  def reversed: Map[Tag, Set[Word]] = ???

  def entries: Map[Word, Set[Tag]] = map.mapVals(_ -- excludedTags).filter(_._2.nonEmpty)
  def knownWordsForTag: Map[Tag, Set[Word]] = allTags.mapToVal(Set.empty[Word]).toMap ++ entries.ungroup.map(_.swap).groupByKey.mapVals(_.toSet)

  def withWords(words: Set[Word]) = new SimpleTagDictionary(map, allWords ++ words, allTags -- excludedTags, startWord, startTag, endWord, endTag, excludedTags)
  def withTags(tags: Set[Tag]) = new SimpleTagDictionary(map, allWords, (allTags ++ tags) -- excludedTags, startWord, startTag, endWord, endTag, excludedTags)
  def withExcludedTags(tags: Set[Tag]) = new SimpleTagDictionary(map, allWords, allTags, startWord, startTag, endWord, endTag, excludedTags ++ tags)
}

object SimpleTagDictionary {
  def apply[Word, Tag](
    map: Map[Word, Set[Tag]],
    startWord: Word, startTag: Tag, endWord: Word, endTag: Tag,
    additionalWords: Set[Word] = Set[Word](), additionalTags: Set[Tag] = Set[Tag](),
    excludedTags: Set[Tag] = Set[Tag]()) = {

    val allAllWords = additionalWords ++ map.keys
    val allAllTags = additionalTags ++ map.flatMap(_._2) -- excludedTags
    new SimpleTagDictionary(
      map.mapVals(_ -- excludedTags) ++ Map(startWord -> Set(startTag), endWord -> Set(endTag)),
      allAllWords - (startWord, endWord), allAllTags -- excludedTags - (startTag, endTag),
      startWord, startTag, endWord, endTag,
      excludedTags)
  }

  def empty[Word, Tag](startWord: Word, startTag: Tag, endWord: Word, endTag: Tag, excludedTags: Set[Tag] = Set[Tag]()) = {
    SimpleTagDictionary(Map(), startWord, startTag, endWord, endTag, excludedTags = excludedTags)
  }
}

trait TagDictionaryFactory[Word, Tag] {
  def apply[Word, Tag](
    sentences: Vector[Vector[(Word, Tag)]],
    startWord: Word, startTag: Tag, endWord: Word, endTag: Tag,
    additionalWords: Set[Word] = Set[Word](), additionalTags: Set[Tag] = Set[Tag](),
    excludedTags: Set[Tag] = Set[Tag]() //
    ): TagDictionary[Word, Tag]
}

class SimpleTagDictionaryFactory[Word, Tag](tdCutoff: Option[Double] = None) extends TagDictionaryFactory[Word, Tag] {
  override def apply[Word, Tag](
    taggedSentences: Vector[Vector[(Word, Tag)]],
    startWord: Word, startTag: Tag, endWord: Word, endTag: Tag,
    additionalWords: Set[Word], additionalTags: Set[Tag],
    excludedTags: Set[Tag] = Set[Tag]()) = {
    val tagCounts = taggedSentences.flatten.groupByKey.mapVals(_.counts.normalizeValues)
    val cutoff = tdCutoff.getOrElse(0.0)
    val pruned = tagCounts.mapVals(_.collect { case (t, p) if p >= cutoff => t }.toSet -- excludedTags).filter(_._2.nonEmpty)
    SimpleTagDictionary(pruned, startWord, startTag, endWord, endTag,
      additionalWords ++ tagCounts.keys,
      additionalTags -- excludedTags,
      excludedTags)
  }
}
