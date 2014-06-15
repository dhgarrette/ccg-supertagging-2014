package dhg.ccg.tag.learn

import org.junit.Test
import dhg.util.StringUtil._
import dhg.util.CollectionUtil._
import math.{ abs, pow }
import org.junit.Assert._
import Double.NaN
import dhg.ccg.tag.learn._
import dhg.ccg.tag._
import dhg.ccg.prob._

class EmHmmTests {

  @Test
  def test_EmHmm_toy_train_2iterations {
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

    val trInit =
      new SimpleConditionalProbabilityDistribution[Symbol, Symbol](
        Map(
          tagdict.startTag -> new SimpleProbabilityDistribution(Map('D -> 0.7, 'N -> 0.2, 'V -> 0.1)),
          'D -> new SimpleProbabilityDistribution(Map('D -> 0.1, 'N -> 0.7, 'V -> 0.1, tagdict.endTag -> 0.1)),
          'N -> new SimpleProbabilityDistribution(Map('D -> 0.1, 'N -> 0.3, 'V -> 0.4, tagdict.endTag -> 0.2)),
          'V -> new SimpleProbabilityDistribution(Map('D -> 0.3, 'N -> 0.2, 'V -> 0.1, tagdict.endTag -> 0.4)),
          tagdict.endTag -> ProbabilityDistribution.empty))
    val emInit =
      new SimpleConditionalProbabilityDistribution[Symbol, String](
        Map(
          'D -> new SimpleProbabilityDistribution(Map("the" -> 0.4, "a" -> 0.3, "dog" -> 0.1, "chases" -> 0.1, "runs" -> 0.1)),
          'N -> new SimpleProbabilityDistribution(Map("cat" -> 0.2, "man" -> 0.3, "dog" -> 0.2, "chases" -> 0.2, "runs" -> 0.1)),
          'V -> new SimpleProbabilityDistribution(Map("man" -> 0.2, "dog" -> 0.1, "walks" -> 0.2, "chases" -> 0.3, "runs" -> 0.2)),
          tagdict.startTag -> new SimpleProbabilityDistribution(Map(tagdict.startWord -> 1.0)),
          tagdict.endTag -> new SimpleProbabilityDistribution(Map(tagdict.endWord -> 1.0))))

    //    val emt = new SimpleTypeSupervisedTrainer(new EmHmmTrainer(2, 1e-10), trInitializer, emInitializer)
    //    val hmm = emt.train(sentences, tagdict).asInstanceOf[HmmTagger[String, Symbol]]

    val emt = new EmHmmTaggerTrainer[String, Symbol](2, new UnsmoothedTransitionDistributioner[String, Symbol](), new UnsmoothedEmissionDistributioner[String, Symbol](), alphaT = 0.0, alphaE = 0.0, 1e-10)
    val hmm = emt.train(sentences, tagdict, trInit, emInit).asInstanceOf[HmmTagger[String, Symbol]]

    val tr = hmm.transitions
    val em = hmm.emissions

    //    for (t1 <- tagdict.allTags.toVector.sortBy(_.toString))
    //      println(f"$t1 -> ${tagdict.allTags.toVector.sortBy(_.toString).mapTo(t2 => f"${tr(t2, t1)}%.2f").mkString(" ")}")
    //    for (t <- tagdict.allTags.toVector.sortBy(_.toString))
    //      println(f"$t -> ${tagdict.allWords.toVector.sortBy(_.toString).mapTo(w => f"${em(w, t)}%.2f").mkString(" ")}")

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

  @Test
  def test_EmHmm_icecream_doTrain_2iterations {
    val scale = 10

    val s1 = Array(-1, 2, 3, 3, 2, 3, 2, 3, 2, 2, 3, 1, 3, 3, 1, 1, 1, 2, 1, 1, 1, 3, 1, 2, 1, 1, 1, 2, 3, 3, 2, 3, 2, 2, 0)
    val s2 = Array(-1, 2, 3, 3, 2, 3, 2, 3, 2, 2, 3, 1, 3, 3, 1, 1, 1, 2, 1, 1, 1, 3, 1, 2, 1, 1, 1, 2, 3, 3, 2, 3, 2, 2, 0)
    val td = Array(0) +: Array(1) +: Array.fill(3)(Array(2, 3))

    val emt = new EmHmmTaggerTrainer(2, new UnsmoothedTransitionDistributioner[String, String](), new UnsmoothedEmissionDistributioner[String, String](), alphaT = 0.0, alphaE = 0.0, 1e-10)
    val (learnedTr, learnedEm) =
      emt.doTrain(
        Vector(s1, s2).map(s => (s.map(_ + 1), s.map(td))),
        numWords = 5, numTags = 4,
        rtd = Array(0) +: Array(1) +: Array.fill(2)(Array(2, 3, 4)),
        alphaPriorTr =
          Array(
            Array(0.0, 0.0, 0.0, 0.0), //  C_t(.|START)
            Array(0.0, 0.0, 0.0, 0.0), //  C_t(.|END) 
            Array(0.0, 0.0, 0.0, 0.0), //  C_t(.|C)
            Array(0.0, 0.0, 0.0, 0.0)), // C_t(.|H)
        alphaPriorEm =
          Array(
            Array(0.0, 0.0, 0.0, 0.0, 0.0), //  C_e(.|START)
            Array(0.0, 0.0, 0.0, 0.0, 0.0), //  C_e(.|END)
            Array(0.0, 0.0, 0.0, 0.0, 0.0), //  C_e(.|C)
            Array(0.0, 0.0, 0.0, 0.0, 0.0)), // C_e(.|H)
        logInitialTr = Array(
          Array(0.0, 0.0, 0.5, 0.5), //  p_t(.|START)
          Array(0.0, 0.0, 0.0, 0.0), //  p_t(.|END) 
          Array(0.0, 0.1, 0.7, 0.2), //  p_t(.|C)
          Array(0.0, 0.1, 0.1, 0.8)), // p_t(.|H)
        logInitialEm = Array(
          Array(1.0, 0.0, 0.0, 0.0, 0.0), //  p_e(.|START)
          Array(0.0, 1.0, 0.0, 0.0, 0.0), //  p_e(.|END)
          Array(0.0, 0.0, 0.7, 0.2, 0.1), //  p_e(.|C)
          Array(0.0, 0.0, 0.1, 0.3, 0.6))) // p_e(.|H)
    assertEquals2dArray(Array(
      Array(0.0, 0.0, 0.019187795922706, 0.980812204077294), //  p_t(.|START)
      Array(NaN, NaN, NaN, NaN), //  p_t(.|END) 
      Array(0.0, 0.001218263806694, 0.885892498643987, 0.112889237549318), //  p_t(.|C)
      Array(0.0, 0.052351859830148, 0.085481356704799, 0.862166783465053)), // p_t(.|H)
      learnedTr.map(_.map { case Double.NaN => 0.0; case x => x }.toVector.normalize.toArray), scale)
    assertEquals2dArray(Array(
      Array(NaN, NaN, NaN, NaN, NaN), //  p_e(.|START)
      Array(NaN, NaN, NaN, NaN, NaN), //  p_e(.|END)
      Array(NaN, NaN, 0.714543877350896, 0.168586118793123, 0.116870003855980), //  p_e(.|C)
      Array(NaN, NaN, 0.044341970791651, 0.458226316363542, 0.497431712844807)), // p_e(.|H)
      learnedEm.map(_.map { case Double.NaN => 0.0; case x => x }.toVector.normalize.toArray), scale)
  }

  @Test
  def test_EmHmm_icecream_doTrain_11iterations {
    val scale = 3

    val s1 = Array(-1, 2, 3, 3, 2, 3, 2, 3, 2, 2, 3, 1, 3, 3, 1, 1, 1, 2, 1, 1, 1, 3, 1, 2, 1, 1, 1, 2, 3, 3, 2, 3, 2, 2, 0)
    val s2 = Array(-1, 2, 3, 3, 2, 3, 2, 3, 2, 2, 3, 1, 3, 3, 1, 1, 1, 2, 1, 1, 1, 3, 1, 2, 1, 1, 1, 2, 3, 3, 2, 3, 2, 2, 0)
    val td = Array(0) +: Array(1) +: Array.fill(3)(Array(2, 3))

    val emt = new EmHmmTaggerTrainer(11, new UnsmoothedTransitionDistributioner[String, String](), new UnsmoothedEmissionDistributioner[String, String](), alphaT = 0.0, alphaE = 0.0, 1e-10)
    val (learnedTr, learnedEm) =
      emt.doTrain(
        Vector(s1, s2).map(s => (s.map(_ + 1), s.map(td))),
        numWords = 5, numTags = 4,
        rtd = Array(0) +: Array(1) +: Array.fill(2)(Array(2, 3, 4)),
        alphaPriorTr =
          Array(
            Array(0.0, 0.0, 0.0, 0.0), //  C_t(.|START)
            Array(0.0, 0.0, 0.0, 0.0), //  C_t(.|END) 
            Array(0.0, 0.0, 0.0, 0.0), //  C_t(.|C)
            Array(0.0, 0.0, 0.0, 0.0)), // C_t(.|H)
        alphaPriorEm =
          Array(
            Array(0.0, 0.0, 0.0, 0.0, 0.0), //  C_e(.|START)
            Array(0.0, 0.0, 0.0, 0.0, 0.0), //  C_e(.|END)
            Array(0.0, 0.0, 0.0, 0.0, 0.0), //  C_e(.|C)
            Array(0.0, 0.0, 0.0, 0.0, 0.0)), // C_e(.|H)
        Array(
          Array(0.0, 0.0, 0.5, 0.5), //  p_t(.|START)
          Array(0.0, 0.0, 0.0, 0.0), //  p_t(.|END) 
          Array(0.0, 0.1, 0.7, 0.2), //  p_t(.|C)
          Array(0.0, 0.1, 0.1, 0.8)), // p_t(.|H)
        Array(
          Array(1.0, 0.0, 0.0, 0.0, 0.0), //  p_e(.|START)
          Array(0.0, 1.0, 0.0, 0.0, 0.0), //  p_e(.|END)
          Array(0.0, 0.0, 0.7, 0.2, 0.1), //  p_e(.|C)
          Array(0.0, 0.0, 0.1, 0.3, 0.6))) // p_e(.|H)
    assertEquals2dArray(Array(
      Array(0.0, 0.0, 2.741248327961E-13, 0.999999999999726), //  p_t(.|START)
      Array(NaN, NaN, NaN, NaN), //  p_t(.|END) 
      Array(0.0, 1.824185761141E-14, 0.933701716226397, 0.066298283773585), //  p_t(.|C)
      Array(0.0, 0.063135967746579, 0.071833324323886, 0.865030707929535)), // p_t(.|H)
      learnedTr.map(_.map { case Double.NaN => 0.0; case x => x }.toVector.normalize.toArray), scale)
    assertEquals2dArray(Array(
      Array(NaN, NaN, NaN, NaN, NaN), //  p_e(.|START)
      Array(NaN, NaN, NaN, NaN, NaN), //  p_e(.|END)
      Array(NaN, NaN, 0.640784802222534, 0.148092649969018, 0.211122547808448), //  p_e(.|C)
      Array(NaN, NaN, 2.136834492453E-4, 0.534039193707180, 0.465747122843575)), // p_e(.|H)
      learnedEm.map(_.map { case Double.NaN => 0.0; case x => x }.toVector.normalize.toArray), scale)
  }

  def assertEquals2dArray(expected: Array[Array[Double]], result: Array[Array[Double]], scale: Int) {
    //    println("\n" + g(expected, result))
    if (expected.size != result.size)
      fail("\n" + g(expected, result, scale))
    for (((a, b), i) <- (expected zip result).zipWithIndex) {
      if (a.size != b.size)
        fail("\n" + g(expected, result, scale))
      for (((x, y), j) <- (a zip b).zipWithIndex) {
        assertTrue(f"[$i,$j]: $x != $y\n" + g(expected, result, scale), x.isNaN || abs(x - y) < pow(0.1, scale))
      }
    }
  }

  def f(a: Array[Array[Double]], scale: Int) = {
    val m1 = a.map(_.map(v => if (v == NaN) "" else f"%%.${scale}f".format(v)))
    val maxl = if (scale > 0) scale + 2 else m1.flatten.map(_.length).max
    m1.map(_.map(_.padRight(maxl)).mkString("[", ", ", "]")).mkString("\n")
  }
  def g(ex: Array[Array[Double]], re: Array[Array[Double]], scale: Int) =
    sideBySideStrings(1, "expected:", f(ex, scale), "   ", "result:", f(re, scale))

}
