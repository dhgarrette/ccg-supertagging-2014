package dhg.ccg.tag

import dhg.util.CollectionUtil._
import dhg.util.FileUtil._
import dhg.util.StringUtil._
import com.typesafe.scalalogging.slf4j.{ StrictLogging => Logging }
import dhg.util.Time
import dhg.util.CommandLineUtil
import math.{ log, exp }
import scalaz._
import Scalaz._
import dhg.ccg.prob._
import dhg.ccg.tag.learn._
import dhg.util.math.LogDouble

class HmmTagger[Word, Tag](
  val transitions: ConditionalProbabilityDistribution[Tag, Tag],
  val emissions: ConditionalProbabilityDistribution[Tag, Word],
  val tagdict: TagDictionary[Word, Tag])
  extends WeightedTagger[Word, Tag]
  with Tagger[Word, Tag]
  with Logging {

  /**
   * Compute the probability of the tagged sentence.  The result
   * should be represented as a logarithm.
   */
  override def sentenceProbWithWeights(sentence: Vector[(Word, Tag)], us: Vector[Map[Tag, LogDouble]]): LogDouble = {
    (((tagdict.startWord -> tagdict.startTag) +: sentence :+ (tagdict.endWord -> tagdict.endTag)) zipSafe (Map.empty[Tag, LogDouble] +: us :+ Map.empty[Tag, LogDouble]))
      .sliding2.foldLeft(LogDouble.one) {
        case (logProd, (((_, prevTag), _), ((currWord, currTag), u))) =>
          logProd * LogDouble(transitions(currTag, prevTag)) * LogDouble(emissions(currWord, currTag)) / u.getOrElse(currTag, LogDouble.zero)
      }
  }

  /**
   * Accepts a sentence of word tokens and returns a sequence of
   * tags corresponding to each of those words.
   */
  override def tagAndProbWithWeights(sentence: Vector[Word], us: Vector[Map[Tag, LogDouble]]): (Vector[Tag], LogDouble) = {
    tagAndProbWithWeightsFromTagSet(sentence.mapTo(tagdict), us)
  }

  /**
   * Accepts a sentence of word tokens and returns a sequence of
   * tags corresponding to each of those words.
   */
  override def tagAndProbWithWeightsFromTagSet(sentence: Vector[(Word, Set[Tag])], us: Vector[Map[Tag, LogDouble]]): (Vector[Tag], LogDouble) = {
    val forwards =
      ((sentence :+ ((tagdict.endWord, Set(tagdict.endTag)))) zipSafe (us :+ Map.empty))
        .scanLeft(Map(tagdict.startTag -> (LogDouble.one, tagdict.startTag))) {
          case (prevV, ((currWord, potentialTags), u)) =>
            potentialTags.mapTo { k =>
              val scores =
                prevV.map {
                  case (kprime, (kprimeScore, _)) =>
                    val score = kprimeScore * LogDouble(transitions(k, kprime))
                    kprime -> score
                }
              val (bestKprime, bestKprimeScore) = scores.maxBy(_._2)
              (LogDouble(emissions(currWord, k)) * bestKprimeScore / u.getOrElse(k, LogDouble.one), bestKprime)
            }.toMap
        }
    val tags =
      forwards.scanRight(tagdict.endTag) {
        (v, kNext) => v(kNext)._2
      }.drop(2).dropRight(1) // drop start/end tags
    val p = forwards.last(tagdict.endTag)._1
    (tags, p)
  }

}

trait SupervisedHmmTaggerTrainer[Word, Tag] extends SupervisedTaggerTrainer[Word, Tag] {
  def make(
    transitions: ConditionalProbabilityDistribution[Tag, Tag],
    emissions: ConditionalProbabilityDistribution[Tag, Word],
    tagdict: TagDictionary[Word, Tag]) = new HmmTagger(transitions, emissions, tagdict)
}

class UnsmoothedHmmTaggerTrainer[Word, Tag]() extends SupervisedHmmTaggerTrainer[Word, Tag]() {
  val dister = new SmoothedHmmTaggerTrainer[Word, Tag](new UnsmoothedTransitionDistributioner(), new UnsmoothedEmissionDistributioner())
  override def train(taggedSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Word, Tag]): HmmTagger[Word, Tag] = {
    dister.train(taggedSentences, initialTagdict)
  }
}

class AddLambdaSmoothedHmmTaggerTrainer[Word, Tag](lambda: Double) extends SupervisedHmmTaggerTrainer[Word, Tag] {
  val dister = new SmoothedHmmTaggerTrainer[Word, Tag](new AddLambdaTransitionDistributioner(lambda), new AddLambdaEmissionDistributioner(lambda))
  override def train(taggedSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Word, Tag]): HmmTagger[Word, Tag] = {
    dister.train(taggedSentences, initialTagdict)
  }
}

class SmoothedHmmTaggerTrainer[Word, Tag](
  transitionDister: TransitionDistributioner[Word, Tag], emissionDister: EmissionDistributioner[Word, Tag])
  extends SupervisedHmmTaggerTrainer[Word, Tag]() {

  override def train(taggedSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Word, Tag]): HmmTagger[Word, Tag] = {
    val tagdict = initialTagdict.withWords(taggedSentences.flatten.map(_._1).toSet).withTags(taggedSentences.flatten.map(_._2).toSet)

    val transitions = transitionDister(taggedSentences, tagdict)
    val emissions = emissionDister(taggedSentences, tagdict)

    new HmmTagger(transitions, emissions, tagdict)
  }
}

object Hmm extends Logging {

  /** Returns a vector of tagged sentences */
  def taggedSentencesFile(filename: String) = {
    File(filename).readLines.zipWithIndex.map {
      case (line, lineNum) =>
        line.split("\\s+")
          .map(_.split("\\|"))
          .map {
            case Array(w, t) => (w, t)
            case x => sys.error(f"failed on line $lineNum")
          }.toVector
    }.toVector
  }

  def main(args: Array[String]): Unit = {
    val (arguments, options) = CommandLineUtil.parseArgs(args)

    val trainData = taggedSentencesFile(options("train"))

    val td = {
      val tdcutoffProvided = options.get("tdcutoff").isDefined
      val tdCutoff =
        options.get("tagdict").fold(tdcutoffProvided)(_.toBoolean).option {
          options.get("tdcutoff").fold(0.0)(_.toDouble)
        }
      new SimpleTagDictionaryFactory(tdCutoff)(trainData, "<S>", "<S>", "<E>", "<E>")
    }

    val trainer: SupervisedTaggerTrainer[String, String] = {
      val lambda = options.get("lambda").fold(1.0)(_.toDouble)
      if (options.contains("lambda") && !options.contains("tsmooth") && !options.contains("esmooth")) {
        new AddLambdaSmoothedHmmTaggerTrainer[String, String](lambda)
      }
      else if (options.contains("tsmooth") || options.contains("esmooth")) {
        val tsmooth: TransitionDistributioner[String, String] =
          options.getOrElse("tsmooth", "none") match {
            case "addlambda" => new AddLambdaTransitionDistributioner(lambda)
            case "none" | "unsmoothed" | "un" => new UnsmoothedTransitionDistributioner()
          }
        val esmooth: EmissionDistributioner[String, String] =
          options.getOrElse("esmooth", "none") match {
            case "addlambda" => new AddLambdaEmissionDistributioner(lambda)
            case "none" | "unsmoothed" | "un" => new UnsmoothedEmissionDistributioner()
          }
        new SmoothedHmmTaggerTrainer(tsmooth, esmooth)
      }
      else {
        new UnsmoothedHmmTaggerTrainer()
      }
    }

    val model = Time.time("training", trainer.train(trainData, td))
    TaggerEvaluator(model, taggedSentencesFile(options("test")))
  }

}
