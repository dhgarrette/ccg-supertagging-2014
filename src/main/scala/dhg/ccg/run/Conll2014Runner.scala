package dhg.ccg.run

import dhg.util.CollectionUtil._
import dhg.util.Time._
import dhg.util.FileUtil._
import dhg.util.Pattern._
import dhg.util.StringUtil._
import math.{ log, exp, abs }
import com.typesafe.scalalogging.slf4j.{ StrictLogging => Logging }
import scala.collection.immutable.BitSet
import scala.collection.breakOut
import scala.util.Random
import annotation.tailrec
import dhg.util.CommandLineUtil
import scala.util.Try
import scalaz._
import Scalaz._
import dhg.ccg.tag.learn._
import dhg.ccg.data._
import dhg.ccg.prob._
import dhg.ccg.tag.learn._
import dhg.ccg.tag._
import dhg.ccg.tag.learn._
import dhg.ccg.cat._
import org.apache.commons.math3.random.{ MersenneTwister, RandomGenerator }

class Conll2014Runner[Word, Tag](
  bankRunner: Conll2014BankRunner[Word, Tag]) {

  val bankReader = bankRunner.bankReader

  def run(algorithm: String, cmd: String, arguments: Vector[String], options: Map[String, String]): Unit = {
    val algo = algorithm.toUpperCase

    println(cmd); Console.err.println(cmd)

    val tsmooth: TransitionDistributioner[Word, Tag] =
      options.getOrElse("tsmooth", "no") match {
        case x if x.startsWith("un") | x.startsWith("no") => new UnsmoothedTransitionDistributioner()
        case x if x.startsWith("ad") => new AddLambdaTransitionDistributioner(0.1)
      }
    val esmooth: EmissionDistributioner[Word, Tag] =
      options.getOrElse("esmooth", "no") match {
        case x if x.startsWith("un") | x.startsWith("no") => new UnsmoothedEmissionDistributioner()
        case x if x.startsWith("ad") => new AddLambdaEmissionDistributioner(0.1)
      }
    val trInitializer = bankRunner.getTransitionInitializer(options.getOrElse("trinit", "uniform") match {
      case x if x.startsWith("un") => new TrUniform[Word, Tag]()
      case x if x.startsWith("tde") => new TrTagDictEntriesPossibilities[Word, Tag](new AddLambdaTransitionDistributioner(0.1))
    }, arguments, options)
    val emInitializer = bankRunner.getEmissionInitializer(options.getOrElse("eminit", "uniform") match {
      case x if x.startsWith("un") => new EmUniform[Word, Tag]()
      case x if x.startsWith("tde") => new EmTagDictEstimate[Word, Tag](new TagDictEstimateTagPriorInitializer(0.1), 0.1, 0.1) 
    }, arguments, options)

    val maxIterations = options.get("iterations").fold(50)(_.toInt)
    val samplingIterations = options.get("samples").fold(100)(_.toInt)
    val burninIterations = options.get("burnin").fold(100)(_.toInt)
    val trAlpha = options.get("tralpha").fold(3000)(_.toInt)
    val emAlpha = options.get("emalpha").fold(7000)(_.toInt)
    val memmCutoff = options.get("memmcut").fold(100)(_.toInt)
    val tdcut = options.get("tdcut").fold(0.1)(_.toDouble)
    val tdCutoff = Some(tdcut)
    val tdFactory = new SimpleTagDictionaryFactory[Word, Tag](tdCutoff)
    val td = tdFactory(bankReader.tdData, bankReader.startWord, bankReader.startTag, bankReader.endWord, bankReader.endTag)

    val emTrainer = algo match {
      case "EM" => new SimpleTypeSupervisedTaggerTrainer(new EmHmmTaggerTrainer(maxIterations, tsmooth, esmooth, alphaT = trAlpha, alphaE = emAlpha, 1e-10), trInitializer, emInitializer)
      case "FFBS" => new SimpleTypeSupervisedTaggerTrainer(new FfbsHmmTaggerTrainer(samplingIterations, burninIterations, trAlpha, emAlpha, tsmooth, esmooth, new MersenneTwister), trInitializer, emInitializer)
    }
    val meTrainer = new MemmTaggerTrainer(maxIterations, memmCutoff, tdRestricted = true, bankReader.wordToString, bankReader.wordFromString, bankReader.tagToString, bankReader.tagFromString)

    def printInfo() {
      println(algo)
      println(f"trainer: $emTrainer")
      println
      println(f"tagset = ${bankReader.getClass.getName}")
      println
      println(f"td cutoff = $tdcut")
      println(f"td tokens  = ${bankReader.tdData.flatten.size}%6s  (${bankReader.tdData.size} sentences)")
      println(f"td words   = ${td.entries.keySet.size}%6s")
      println(f"td entries = ${td.entries.sumBy(_._2.size)}%6s")
      println
      println(f"raw tokens =  ${bankReader.raw.flatten.size}%6s  (${bankReader.raw.size} sentences)")
      println(f"dev tokens =  ${bankReader.devData.flatten.size}%6s  (${bankReader.devData.size} sentences)")
      println(f"test tokens = ${bankReader.testData.flatten.size}%6s  (${bankReader.testData.size} sentences)")
      println(f"raw data ambiguity (based on td), type =  ${bankReader.raw.flatten.map(w => td(w).size).avg}%.2f")
      println(f"raw data ambiguity (based on td), token = ${bankReader.raw.flatten.distinct.map(w => td(w).size).avg}%.2f")
      println
      println(f"tr initializer = $trInitializer")
      println(f"em initializer = $emInitializer")
      println(f"tr distributioner = $tsmooth")
      println(f"em distributioner = $esmooth")
      println(f"memm cutoff = $memmCutoff")
      println
      algo match {
        case "EM" =>
          println(f"max iterations = $maxIterations")
        case "FFBS" =>
          println(f"sampling iterations = $samplingIterations")
          println(f"burn-in iterations = $burninIterations")
          println(f"tr alpha = $trAlpha")
          println(f"em alpha = $emAlpha")
      }
    }
    printInfo()

    val raw = bankReader.raw
    /* train an HMM directly on initialization distributions (without EM) */
    val hmm = new HmmTagger(trInitializer(raw, td), emInitializer(raw, td), td)
    /* learn a smoothed HMM from EM */
    val emHmm = emTrainer.typesupTrain(raw, td)
    /* learn an MEMM from auto-tagged data produced by the smoothed HMM */
    val memm = meTrainer.train(raw.map(s => s zipSafe emHmm.tag(s)), td)

    printInfo()

    val testData = bankReader.devData
    println(f"\nNo $algo")
    val a = TaggerEvaluator(hmm, testData)
    println(f"\nSmoothed HMM from $algo")
    val b = TaggerEvaluator(emHmm, testData)
    println(f"\nMEMM trained from auto-tagged data produced by the smoothed HMM")
    val c = TaggerEvaluator(memm, testData)
    println(cmd)
    println(cmd.replaceAll("\\s+|\\.", "_"))
    println(f"${cmd.replaceAll("\\s+", "\t").replaceAll("\\t--", "\t'--")}\t$a\t$b\t$c")
  }

}

trait Conll2014BankRunner[Word, Tag] {
  def bankReader: BankReader[Word, Tag]
  def getTransitionInitializer(trInitializer: TransitionInitializer[Word, Tag], arguments: Vector[String], options: Map[String, String]): TransitionInitializer[Word, Tag] = trInitializer
  def getEmissionInitializer(emInitializer: EmissionInitializer[Word, Tag], arguments: Vector[String], options: Map[String, String]): EmissionInitializer[Word, Tag] = emInitializer
}

class Conll2014SimpleCcgBankRunner(val bankReader: BankReader[String, String]) extends Conll2014BankRunner[String, String] {
}

class Conll2014CatCcgBankRunner(val bankReader: BankReader[String, Cat]) extends Conll2014BankRunner[String, Cat] {
  val WRe = "w(.+)".r
  val TRe = "t(.+)".r
  val FRe = "f(.+)".r
  val MRe = "m(.+)".r
  val LRe = "l(.+)".r

  private[this] def getCatPriorInit(options: Map[String, String]) = {
    options.get("ccgtrinit-catprior").fold(none[(TagPriorInitializer[String, Cat], Double)]) { v =>
      v.lsplit(",") match {
        case Seq("cplx", WRe(UDouble(catpriorMass))) =>
          Some((new CatComplexityInitializer[String], catpriorMass))
        case Seq("pcfg", TRe(UDouble(pTerm)), FRe(UDouble(pFwd)), MRe(UDouble(pMod)), WRe(UDouble(catpriorMass))) =>
          Some((new UniformCatgramAtomCatPriorInitializer[String](pTerm, pMod, pFwd), catpriorMass))
        case Seq("tdipcfg", TRe(UDouble(pTerm)), FRe(UDouble(pFwd)), MRe(UDouble(pMod)), WRe(UDouble(catpriorMass)), rest @ _*) =>
          val atomLambda = rest match { case Seq(LRe(UDouble(atomLambda))) => atomLambda; case Seq() => 1.0 }
          Some((new TagdictInformedCatgramCatPriorInitializer[String](pTerm, pMod, pFwd, atomLambda), catpriorMass))
        case Seq("false") => None
      }
    }
  }

  override def getTransitionInitializer(trInitializer: TransitionInitializer[String, Cat], arguments: Vector[String], options: Map[String, String]) = {
    val a =
      options.get("ccgtrinit-comb").fold(none[Double])(v => Try(v.toDouble).toOption).map { combMass =>
        val canCombine = new SimpleCatCanCombine(Vector(FA, FAn2np, BA, BAn2np, FC, BC, /*FX,*/ BX), bankReader.startTag, bankReader.endTag)
        new CcgCombinabilityTrInitializer(trInitializer, canCombine, combMass)
      }.getOrElse(trInitializer)
    val b = options.get("ccgtrinit-catprior").fold(none[TransitionInitializer[String, Cat]]) { v =>
      getCatPriorInit(options).map {
        case (catpriorInit, catpriorMass) =>
          val catPriorTrInit = new TagPriorTrInitializer(catpriorInit)
          new InterpolatingTransitionInitializer[String, Cat](Vector(a -> (1 - catpriorMass), catPriorTrInit -> catpriorMass))
      }
    }.getOrElse(a)
    b
  }

  override def getEmissionInitializer(emInitializer: EmissionInitializer[String, Cat], arguments: Vector[String], options: Map[String, String]): EmissionInitializer[String, Cat] = {
    getCatPriorInit(options).fold(emInitializer) {
      case (catpriorInit, catpriorMass) =>
        options.getOrElse("eminit", "uniform") match {
          case x if x.startsWith("tde") => new EmTagDictEstimate[String, Cat](catpriorInit, 0.1, 0.1) 
          case _ => emInitializer
        }
    }
  }

}

//
//
//

object Conll2014Run {
  def main(args: Array[String]): Unit = {
    val (arguments, options) = CommandLineUtil.parseArgs(args)
    arguments match {
      case Seq(algorithm, tagset) =>
        val runner = tagset match {
          case "ccgpos" =>
            new Conll2014Runner(new Conll2014SimpleCcgBankRunner(PosEnglishCcgBankReader))
          case "ccg" =>
            new Conll2014Runner(new Conll2014CatCcgBankRunner(FeaturelessEnglishCcgBankReader))
          case "ccgfeat" =>
            new Conll2014Runner(new Conll2014CatCcgBankRunner(FeaturedEnglishCcgBankReader))
          case "tut" =>
            new Conll2014Runner(new Conll2014CatCcgBankRunner(FeaturelessTutCcgBankReader))
          case "tutfeat" =>
            new Conll2014Runner(new Conll2014CatCcgBankRunner(FeaturedTutCcgBankReader))
          case "ctb" =>
            new Conll2014Runner(new Conll2014CatCcgBankRunner(FeaturelessChineseCcgBankReader))
          case "ctbfeat" =>
            new Conll2014Runner(new Conll2014CatCcgBankRunner(FeaturedChineseCcgBankReader))
        }
        runner.run(algorithm, args.mkString(" "), arguments, options)
    }
  }
}
