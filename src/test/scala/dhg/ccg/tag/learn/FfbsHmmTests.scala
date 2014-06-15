package dhg.ccg.tag.learn

import org.junit.Test
import dhg.util.StringUtil._
import dhg.util.CollectionUtil._
import org.junit.Assert._
import dhg.ccg.tag.learn._
import dhg.ccg.prob._
import dhg.ccg.tag._
import org.apache.commons.math3.random.{ MersenneTwister, RandomGenerator }
import dhg.ccg.test.TestUtil.MockableRandomGenerator

class FfbsHmmTests {

  @Test
  def test_toy_train_5iterations {
    val scale = 10

    val sentences = Vector(
      "a cat chases the dog",
      "the dog walks",
      "the man walks the dog",
      "the man runs").map(_.lsplit(" "))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set('D),
      "a" -> Set('D),
      "every" -> Set('D),
      "some" -> Set('D),
      "man" -> Set('N, 'V),
      "cat" -> Set('N),
      "bird" -> Set('N),
      "fox" -> Set('N),
      "walks" -> Set('V),
      "flies" -> Set('N, 'V)),
      "<S>", 'A, "<E>", 'Z)
      .withWords(sentences.flatten.toSet)

    val trInitializer = new TransitionInitializer[String, Symbol] {
      override def apply(sentences: Vector[Vector[String]], itd: TagDictionary[String, Symbol]) = {
        new SimpleConditionalProbabilityDistribution(
          Map(
            itd.startTag -> new SimpleProbabilityDistribution(Map('D -> 0.7, 'N -> 0.2, 'V -> 0.1)),
            'D -> new SimpleProbabilityDistribution(Map('D -> 0.1, 'N -> 0.7, 'V -> 0.1, itd.endTag -> 0.1)),
            'N -> new SimpleProbabilityDistribution(Map('D -> 0.1, 'N -> 0.3, 'V -> 0.4, itd.endTag -> 0.2)),
            'V -> new SimpleProbabilityDistribution(Map('D -> 0.3, 'N -> 0.2, 'V -> 0.1, itd.endTag -> 0.4)),
            itd.endTag -> ProbabilityDistribution.empty))
      }
    }
    val emInitializer = new EmissionInitializer[String, Symbol] {
      override def apply(sentences: Vector[Vector[String]], itd: TagDictionary[String, Symbol]) = {
        new SimpleConditionalProbabilityDistribution(
          Map(
            'D -> new SimpleProbabilityDistribution(Map("the" -> 0.4, "a" -> 0.3, "dog" -> 0.1, "chases" -> 0.1, "runs" -> 0.1)),
            'N -> new SimpleProbabilityDistribution(Map("cat" -> 0.2, "man" -> 0.3, "dog" -> 0.2, "chases" -> 0.2, "runs" -> 0.1)),
            'V -> new SimpleProbabilityDistribution(Map("man" -> 0.2, "dog" -> 0.1, "walks" -> 0.2, "chases" -> 0.3, "runs" -> 0.2)),
            tagdict.startTag -> new SimpleProbabilityDistribution(Map(tagdict.startWord -> 1.0)),
            tagdict.endTag -> new SimpleProbabilityDistribution(Map(tagdict.endWord -> 1.0))))
      }
    }

    class RandomGeneratorDoubleDisplay(rg: RandomGenerator) extends MockableRandomGenerator {
      var d = rg.nextDouble
      override def nextDouble = { val temp = d; d = rg.nextDouble; temp }
      override def toString = f"RG(next=$d%.2f)"
    }

    val rand = new RandomGeneratorDoubleDisplay(new MersenneTwister(0))

    val emt = new SimpleTypeSupervisedTaggerTrainer(new FfbsHmmTaggerTrainer(3, 2, 1, 1, new UnsmoothedTransitionDistributioner[String, Symbol](), new UnsmoothedEmissionDistributioner[String, Symbol](), rand), trInitializer, emInitializer)
    val hmm = emt.typesupTrain(sentences, tagdict).asInstanceOf[HmmTagger[String, Symbol]]

    val tr = hmm.transitions
    val em = hmm.emissions

    for (t1 <- tagdict.allTags.toVector.sortBy(_.toString))
      println(f"$t1 -> ${tagdict.allTags.toVector.sortBy(_.toString).mapTo(t2 => f"${tr(t2, t1)}%.2f").mkString(" ")}")
    for (t <- tagdict.allTags.toVector.sortBy(_.toString))
      println(f"$t -> ${tagdict.allWords.toVector.sortBy(_.toString).mapTo(w => f"${em(w, t)}%.2f").mkString(" ")}")

    ???

    assertEquals(0.0, tr('A, 'A), 1e-9)
    assertEquals(1.0, tr('D, 'A), 1e-9)
    assertEquals(0.0, tr('N, 'A), 1e-9)
    assertEquals(0.0, tr('V, 'A), 1e-9)
    assertEquals(0.0, tr('Z, 'A), 1e-9)

    assertEquals(0.0, tr('A, 'D), 1e-9)
    assertEquals(0.0000212112, tr('D, 'D), 1e-9)
    assertEquals(0.9951962019, tr('N, 'D), 1e-9)
    assertEquals(0.0047679144, tr('V, 'D), 1e-9)
    assertEquals(0.0000146724, tr('Z, 'D), 1e-9)

    assertEquals(0.0, tr('A, 'N), 1e-9)
    assertEquals(0.0001151986, tr('D, 'N), 1e-9)
    assertEquals(0.0011682076, tr('N, 'N), 1e-9)
    assertEquals(0.6678369319, tr('V, 'N), 1e-9)
    assertEquals(0.3308796618, tr('Z, 'N), 1e-9)

    assertEquals(0.0, tr('A, 'V), 1e-9)
    assertEquals(0.4971650599, tr('D, 'V), 1e-9)
    assertEquals(0.0000002073, tr('N, 'V), 1e-9)
    assertEquals(0.0000863669, tr('V, 'V), 1e-9)
    assertEquals(0.5027483659, tr('Z, 'V), 1e-9)

    assertEquals(0.0, tr('A, 'Z), 1e-9)
    assertEquals(0.0, tr('D, 'Z), 1e-9)
    assertEquals(0.0, tr('N, 'Z), 1e-9)
    assertEquals(0.0, tr('V, 'Z), 1e-9)
    assertEquals(0.0, tr('Z, 'Z), 1e-9)

    assertEquals(0.0, tr('A, 'A), 1e-9)
    assertEquals(1.0, tr('D, 'A), 1e-9)
    assertEquals(0.0, tr('N, 'A), 1e-9)
    assertEquals(0.0, tr('V, 'A), 1e-9)
    assertEquals(0.0, tr('Z, 'A), 1e-9)

    assertEquals(0.0, tr('A, 'D), 1e-9)
    assertEquals(0.0000212112, tr('D, 'D), 1e-9)
    assertEquals(0.9951962019, tr('N, 'D), 1e-9)
    assertEquals(0.0047679144, tr('V, 'D), 1e-9)
    assertEquals(0.0000146724, tr('Z, 'D), 1e-9)

    assertEquals(0.0, tr('A, 'N), 1e-9)
    assertEquals(0.0001151986, tr('D, 'N), 1e-9)
    assertEquals(0.0011682076, tr('N, 'N), 1e-9)
    assertEquals(0.6678369319, tr('V, 'N), 1e-9)
    assertEquals(0.3308796618, tr('Z, 'N), 1e-9)

    assertEquals(0.0, tr('A, 'V), 1e-9)
    assertEquals(0.4971650599, tr('D, 'V), 1e-9)
    assertEquals(0.0000002073, tr('N, 'V), 1e-9)
    assertEquals(0.0000863669, tr('V, 'V), 1e-9)
    assertEquals(0.5027483659, tr('Z, 'V), 1e-9)

    assertEquals(0.0, tr('A, 'Z), 1e-9)
    assertEquals(0.0, tr('D, 'Z), 1e-9)
    assertEquals(0.0, tr('N, 'Z), 1e-9)
    assertEquals(0.0, tr('V, 'Z), 1e-9)
    assertEquals(0.0, tr('Z, 'Z), 1e-9)

    assertEquals(1.0, em("<S>", 'A), 1e-9)
    assertEquals(0.0, em("a", 'A), 1e-9)
    assertEquals(0.0, em("cat", 'A), 1e-9)
    assertEquals(0.0, em("chases", 'A), 1e-9)
    assertEquals(0.0, em("the", 'A), 1e-9)
    assertEquals(0.0, em("dog", 'A), 1e-9)
    assertEquals(0.0, em("walks", 'A), 1e-9)
    assertEquals(0.0, em("man", 'A), 1e-9)
    assertEquals(0.0, em("runs", 'A), 1e-9)
    assertEquals(0.0, em("bird", 'A), 1e-9)
    assertEquals(0.0, em("<E>", 'A), 1e-9)

    assertEquals(0.0, em("<S>", 'D), 1e-9)
    assertEquals(0.1666620188, em("a", 'D), 1e-9)
    assertEquals(0.0, em("cat", 'D), 1e-9)
    assertEquals(0.0000055693, em("chases", 'D), 1e-9)
    assertEquals(0.8333100939, em("the", 'D), 1e-9)
    assertEquals(0.0000156419, em("dog", 'D), 1e-9)
    assertEquals(0.0, em("walks", 'D), 1e-9)
    assertEquals(0.0, em("man", 'D), 1e-9)
    assertEquals(0.0000066761, em("runs", 'D), 1e-9)
    assertEquals(0.0, em("bird", 'D), 1e-9)
    assertEquals(0.0, em("<E>", 'D), 1e-9)

    assertEquals(0.0, em("<S>", 'N), 1e-9)
    assertEquals(0.0, em("a", 'N), 1e-9)
    assertEquals(0.1672708350, em("cat", 'N), 1e-9)
    assertEquals(0.0001031622, em("chases", 'N), 1e-9)
    assertEquals(0.0, em("the", 'N), 1e-9)
    assertEquals(0.4970417101, em("dog", 'N), 1e-9)
    assertEquals(0.0, em("walks", 'N), 1e-9)
    assertEquals(0.3345191078, em("man", 'N), 1e-9)
    assertEquals(0.0010651849, em("runs", 'N), 1e-9)
    assertEquals(0.0, em("bird", 'N), 1e-9)
    assertEquals(0.0, em("<E>", 'N), 1e-9)

    assertEquals(0.0, em("<S>", 'V), 1e-9)
    assertEquals(0.0, em("a", 'V), 1e-9)
    assertEquals(0.0, em("cat", 'V), 1e-9)
    assertEquals(0.2485015068, em("chases", 'V), 1e-9)
    assertEquals(0.0, em("the", 'V), 1e-9)
    assertEquals(0.0070688787, em("dog", 'V), 1e-9)
    assertEquals(0.4973263522, em("walks", 'V), 1e-9)
    assertEquals(0.0000335407, em("man", 'V), 1e-9)
    assertEquals(0.2470697217, em("runs", 'V), 1e-9)
    assertEquals(0.0, em("bird", 'V), 1e-9)
    assertEquals(0.0, em("<E>", 'V), 1e-9)

    assertEquals(0.0, em("<S>", 'Z), 1e-9)
    assertEquals(0.0, em("a", 'Z), 1e-9)
    assertEquals(0.0, em("cat", 'Z), 1e-9)
    assertEquals(0.0, em("chases", 'Z), 1e-9)
    assertEquals(0.0, em("the", 'Z), 1e-9)
    assertEquals(0.0, em("dog", 'Z), 1e-9)
    assertEquals(0.0, em("walks", 'Z), 1e-9)
    assertEquals(0.0, em("man", 'Z), 1e-9)
    assertEquals(0.0, em("runs", 'Z), 1e-9)
    assertEquals(0.0, em("bird", 'Z), 1e-9)
    assertEquals(1.0, em("<E>", 'Z), 1e-9)

  }

}
