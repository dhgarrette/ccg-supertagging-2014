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

class Conll2014Trainer[Word](
  catFeatures: Boolean = true,
  combinabilityCcgRules: Vector[TrueCcgRule] = Vector(FA, FAn2np, BA, BAn2np, FC, BC, /*FX,*/ BX),

  pTerm: Double = 0.6,
  pMod: Double = 0.8,
  pFwd: Double = 0.5,
  atomLambda: Double = 1000.0,

  alphaT: Double = 3000, alphaE: Double = 7000,
  samplingIterations: Int = 200, burninIterations: Int = 100,
  maxMemmIterations: Int = 50,

  catpriorMass: Double = 0.5,
  combinableTransitionMass: Double = 0.95,

  startTag: Cat, endTag: Cat,
  wordToString: (Word => String),
  wordFromString: (String => Word),

  rand: RandomGenerator = new MersenneTwister)
  extends TypeSupervisedTaggerTrainer[Word, Cat] {

  val canCombine = new SimpleCatCanCombine(combinabilityCcgRules, startTag, endTag)
  val catprior = new TagdictInformedCatgramCatPriorInitializer[Word](pTerm, pMod, pFwd, atomLambda)
  val catPriorTrInit = new TagPriorTrInitializer(catprior)
  val baseTrInit = new CcgCombinabilityTrInitializer[Word](new TrTagDictEntriesPossibilities(new AddLambdaTransitionDistributioner(0.1)), canCombine, combinableTransitionMass)
  val trInit = new InterpolatingTransitionInitializer[Word, Cat](Vector(catPriorTrInit -> catpriorMass, baseTrInit -> (1 - catpriorMass)))
  val emInit = new EmTagDictEstimate[Word, Cat](catprior, 0.1, 0.1)

  val emTrainer =
    new SimpleTypeSupervisedTaggerTrainer(
      new FfbsHmmTaggerTrainer[Word, Cat](
        samplingIterations, burninIterations,
        alphaT, alphaE,
        new AddLambdaTransitionDistributioner(0.1), new AddLambdaEmissionDistributioner(0.1),
        rand),
      trInit, emInit)

  val meTrainer = new MemmTaggerTrainer[Word, Cat](maxMemmIterations, cutoff = 100, tdRestricted = true, wordToString, wordFromString, _.toString, new CatInterner(removeFeatures = !catFeatures))

  def typesupTrain(rawSentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Word, Cat]) = {
    val tagdict = initialTagdict.withWords(rawSentences.flatten.toSet)

    /* learn a smoothed HMM from EM */
    val emHmm = emTrainer.typesupTrain(rawSentences, tagdict)
    /* learn an MEMM from auto-tagged data produced by the smoothed HMM */
    val memm = meTrainer.train(rawSentences.map(s => s zipSafe emHmm.tag(s)), tagdict)

    memm
  }
}
