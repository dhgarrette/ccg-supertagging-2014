package dhg.ccg.tag.learn

import org.junit.Test
import dhg.util.StringUtil._
import org.junit.Assert._
import dhg.ccg.cat._
import dhg.ccg.prob._
import dhg.ccg.tag.TagDictionary
import dhg.ccg.tag.SimpleTagDictionary

// TODO: Rewrite these to just test CatPriorInitializers, not wrapped in TagPriorTrInitializers
class CcgCatPriorTests {

  
  @Test
  def test_CatComplexityInitializer {

    val s = AtomCat("S")
    val n = AtomCat("N")

    val init = new TagPriorTrInitializer[String, Cat](
      new CatComplexityInitializer())

    val td = SimpleTagDictionary[String, Cat](Map(), "<S>", AtomCat("<S>"), "<E>", AtomCat("<E>"), Set(),
      Set(n, s / s, s \ s, n / n, n \ n, s \ n, (s \ n) / n))

    val sum = Vector(
      1.0 / 1, //<S>
      1.0 / 1, //<E>
      1.0 / 1, //n
      1.0 / 3, //s/s
      1.0 / 3, //s\s
      1.0 / 3, //n/n
      1.0 / 3, //n\n
      1.0 / 3, //s\n
      1.0 / 5 //(s\n)/n
      ).sum

    val tr = init(Vector(), td)

    assertEquals((1.0 / 1) / sum, tr(s, BadCat), 0.000001)
    assertEquals((1.0 / 1) / sum, tr(n, BadCat), 0.000001)
    assertEquals((1.0 / 1) / sum, tr(s("dcl"), BadCat), 0.000001)
    assertEquals((1.0 / 3) / sum, tr(s / n, BadCat), 0.000001)
    assertEquals((1.0 / 3) / sum, tr(s("dcl") / n, BadCat), 0.000001)
    assertEquals((1.0 / 3) / sum, tr(s \ n, BadCat), 0.000001)
    assertEquals((1.0 / 3) / sum, tr(s("dcl") \ n, BadCat), 0.000001)

    assertEquals((1.0 / 3) / sum, tr(n / n, BadCat), 0.000001)
    assertEquals((1.0 / 3) / sum, tr(s \ s, BadCat), 0.000001)
    assertEquals((1.0 / 3) / sum, tr(s("dcl") \ s, BadCat), 0.000001)

    assertEquals((1.0 / 5) / sum, tr((s \ n) / n, BadCat), 0.000001)
    assertEquals((1.0 / 5) / sum, tr((s("dcl") \ n) / n, BadCat), 0.000001)

    assertEquals((1.0 / 7) / sum, tr((s \ n) / (s \ n), BadCat), 0.000001)

    assertEquals((1.0 / 3) / sum, tr(s / n, AtomCat("<S>")), 0.000001)
    assertEquals(0.0, tr(s / n, AtomCat("<E>")), 0.000001)
    assertEquals((1.0 / 1) / sum, tr(AtomCat("<E>"), BadCat), 0.000001)
  }

  
  @Test
  def test_SimpleCatgramCatPriorInitializer {

    val s = AtomCat("S")
    val n = AtomCat("N")

    val init = new TagPriorTrInitializer[String, Cat](
      new SimpleCatgramCatPriorInitializer[String](
        new SimpleProbabilityDistribution(Map(AtomCat("S") -> 0.2, AtomCat("N") -> 0.7, AtomCat("S", Some("dcl")) -> 0.07, AtomCat("<S>") -> 0.01, AtomCat("<E>") -> 0.02)),
        pTerm = 0.75, pMod = 0.4, pFwd = 0.55))

    val td = SimpleTagDictionary[String, Cat](Map(), "<S>", AtomCat("<S>"), "<E>", AtomCat("<E>"), Set(),
      Set(n, s / s, s \ s, n / n, n \ n, s \ n, (s \ n) / n))

    val sum = Vector(
      0.75 * 0.01, //<S>
      0.75 * 0.02, //<E>
      0.75 * 0.7, //n
      (1 - 0.75) * 0.4 * 0.55 * (0.75 * 0.2) * (0.75 * 0.2), //s/s
      (1 - 0.75) * 0.4 * (1 - 0.55) * (0.75 * 0.2) * (0.75 * 0.2), //s\s
      (1 - 0.75) * 0.4 * 0.55 * (0.75 * 0.7) * (0.75 * 0.7), //n/n
      (1 - 0.75) * 0.4 * (1 - 0.55) * (0.75 * 0.7) * (0.75 * 0.7), //n\n
      (1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * 0.2) * (0.75 * 0.7), //s\n
      (1 - 0.75) * (1 - 0.4) * 0.55 * ((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * 0.2) * (0.75 * 0.7)) * (0.75 * 0.7) //(s\n)/n
      ).sum

    val tr = init(Vector(), td)

    assertEquals(0.75 * 0.2 / sum, tr(s, BadCat), 0.000001)
    assertEquals(0.75 * 0.7 / sum, tr(n, BadCat), 0.000001)
    assertEquals(0.75 * 0.07 / sum, tr(s("dcl"), BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * 0.55 * (0.75 * 0.2) * (0.75 * 0.7) / sum, tr(s / n, BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * 0.55 * (0.75 * 0.07) * (0.75 * 0.7) / sum, tr(s("dcl") / n, BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * 0.2) * (0.75 * 0.7) / sum, tr(s \ n, BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * 0.07) * (0.75 * 0.7) / sum, tr(s("dcl") \ n, BadCat), 0.000001)

    assertEquals((1 - 0.75) * 0.4 * 0.55 * (0.75 * 0.7) * (0.75 * 0.7) / sum, tr(n / n, BadCat), 0.000001)
    assertEquals((1 - 0.75) * 0.4 * (1 - 0.55) * (0.75 * 0.2) * (0.75 * 0.2) / sum, tr(s \ s, BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * 0.07) * (0.75 * 0.2) / sum, tr(s("dcl") \ s, BadCat), 0.000001)

    assertEquals((1 - 0.75) * (1 - 0.4) * 0.55 * ((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * 0.2) * (0.75 * 0.7)) * (0.75 * 0.7) / sum, tr((s \ n) / n, BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * 0.55 * ((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * 0.07) * (0.75 * 0.7)) * (0.75 * 0.7) / sum, tr((s("dcl") \ n) / n, BadCat), 0.000001)

    assertEquals((1 - 0.75) * 0.4 * 0.55 * ((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * 0.2) * (0.75 * 0.7)) * ((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * 0.2) * (0.75 * 0.7)) / sum, tr((s \ n) / (s \ n), BadCat), 0.000001)

    assertEquals((1 - 0.75) * (1 - 0.4) * 0.55 * (0.75 * 0.2) * (0.75 * 0.7) / sum, tr(s / n, AtomCat("<S>")), 0.000001)
    assertEquals(0.0, tr(s / n, AtomCat("<E>")), 0.000001)
    assertEquals(0.75 * 0.02 / sum, tr(AtomCat("<E>"), BadCat), 0.000001)
  }

  @Test
  def test_UniformCatgramAtomCatPriorInitializer {
    val s = AtomCat("S")
    val n = AtomCat("N")

    val init = new TagPriorTrInitializer[String, Cat](
      new UniformCatgramAtomCatPriorInitializer[String](
        pTerm = 0.75, pMod = 0.4, pFwd = 0.55))
    //    val init = new TagdictInformedCatPriorTrInitializer[String](
    //      pTerm = 0.75, pMod = 0.4, pFwd = 0.55, atomLambda = 1000000000.0)

    val sentences = Vector(
      "the dog walks",
      "the man walks the dog",
      "the dog runs")
      .map(_.splitWhitespace)
    val td = SimpleTagDictionary[String, Cat](Map(
      "walks" -> Set(s \ n, (s \ n) / n, n),
      "man" -> Set(n),
      "dog" -> Set(n, (s \ n) / n),
      "the" -> Set(n / n),
      "bird" -> Set(n)),
      "<S>", AtomCat("<S>"), "<E>", AtomCat("<E>"), Set(),
      Set(n, s / s, s \ s, n / n, n \ n, s \ n, (s \ n) / n))

    val sum = Vector(
      0.75 * (1.0 / 3), //<E>
      0.75 * (1.0 / 3), //n
      (1 - 0.75) * 0.4 * 0.55 * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3)), //s/s
      (1 - 0.75) * 0.4 * (1 - 0.55) * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3)), //s\s
      (1 - 0.75) * 0.4 * 0.55 * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3)), //n/n
      (1 - 0.75) * 0.4 * (1 - 0.55) * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3)), //n\n
      (1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3)), //s\n
      (1 - 0.75) * (1 - 0.4) * 0.55 * ((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3))) * (0.75 * (1.0 / 3)) //(s\n)/n
      ).sum

    val tr = init(Vector(), td)

    assertEquals(0.0, tr(AtomCat("<S>"), BadCat), 0.000001)
    assertEquals(0.75 * (1.0 / 3) / sum, tr(AtomCat("<E>"), BadCat), 0.000001)

    assertEquals(0.75 * (1.0 / 3) / sum, tr(s, BadCat), 0.000001)
    assertEquals(0.75 * (1.0 / 3) / sum, tr(n, BadCat), 0.000001)
    assertEquals(0.75 * (1.0 / 3) / sum, tr(s("dcl"), BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * 0.55 * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3)) / sum, tr(s / n, BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * 0.55 * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3)) / sum, tr(s("dcl") / n, BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3)) / sum, tr(s \ n, BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3)) / sum, tr(s("dcl") \ n, BadCat), 0.000001)

    assertEquals((1 - 0.75) * 0.4 * 0.55 * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3)) / sum, tr(n / n, BadCat), 0.000001)
    assertEquals((1 - 0.75) * 0.4 * (1 - 0.55) * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3)) / sum, tr(s \ s, BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3)) / sum, tr(s("dcl") \ s, BadCat), 0.000001)

    assertEquals((1 - 0.75) * (1 - 0.4) * 0.55 * ((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3))) * (0.75 * (1.0 / 3)) / sum, tr((s \ n) / n, BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * 0.55 * ((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3))) * (0.75 * (1.0 / 3)) / sum, tr((s("dcl") \ n) / n, BadCat), 0.000001)

    assertEquals((1 - 0.75) * 0.4 * 0.55 * ((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3))) * ((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3))) / sum, tr((s \ n) / (s \ n), BadCat), 0.000001)

    assertEquals((1 - 0.75) * (1 - 0.4) * 0.55 * (0.75 * (1.0 / 3)) * (0.75 * (1.0 / 3)) / sum, tr(s / n, AtomCat("<S>")), 0.000001)
    assertEquals(0.0, tr(s / n, AtomCat("<E>")), 0.000001)
    assertEquals(0.75 * (1.0 / 3) / sum, tr(AtomCat("<E>"), BadCat), 0.000001)
  }

  @Test
  def test_TagdictInformedCatgramCatPriorInitializer {
    val s = AtomCat("S")
    val n = AtomCat("N")
    val np = AtomCat("NP")
    val pp = AtomCat("PP")

    val init = new TagPriorTrInitializer[String, Cat](
      new TagdictInformedCatgramCatPriorInitializer[String](
        pTerm = 0.75, pMod = 0.4, pFwd = 0.55, atomLambda = 2.0))

    val sentences = Vector(
      "the dog walks",
      "the man walks the dog",
      "the dog runs")
      .map(_.splitWhitespace)
    val td = SimpleTagDictionary[String, Cat](Map(
      "walks" -> Set(s \ np, (s \ np) / np, n),
      "man" -> Set(n),
      "dog" -> Set(n, (s \ np) / np),
      "the" -> Set(np / n),
      "bird" -> Set(n)),
      "<S>", AtomCat("<S>"), "<E>", AtomCat("<E>"), Set(),
      Set(n / n, n \ n, pp / n))

    /*
     * walks x3 => s\np, (s\np)/np, n  x3  =>  2*3/3s,  3*3/3np,  1*3/3n
     * man   x2 => n x2                    =>                     1*2/1n
     * dog   x4 => n, (s\np)/np x4         =>  1*4/2s,  2*4/2np,  1*4/2n
     * the   x5 => np/n x5                 =>           1*5/1np,  1*5/1n
     * bird  x1 => n x1                    =>                     1*1/1n
     *                                        --------------------------
     *                                          24/6s    72/6np    66/6n
     *                                             4s      12np      11n
     *                                        -------------------------------------------
     *                                             6s      14np      13n    5<e>   2pp  = 40
     */

    val sum = Vector(
      0.75 * (5 / 40.0), //<E>
      0.75 * (13 / 40.0), //n
      (1 - 0.75) * (1 - 0.4) * 0.55 * (0.75 * (14 / 40.0)) * (0.75 * (13 / 40.0)), //np/n
      (1 - 0.75) * 0.4 * 0.55 * (0.75 * (13 / 40.0)) * (0.75 * (13 / 40.0)), //n/n
      (1 - 0.75) * 0.4 * (1 - 0.55) * (0.75 * (13 / 40.0)) * (0.75 * (13 / 40.0)), //n\n
      (1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (6 / 40.0)) * (0.75 * (14 / 40.0)), //s\np
      (1 - 0.75) * (1 - 0.4) * 0.55 * ((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (6 / 40.0)) * (0.75 * (14 / 40.0))) * (0.75 * (14 / 40.0)), //(s\np)/np
      (1 - 0.75) * (1 - 0.4) * 0.55 * (0.75 * (2 / 40.0)) * (0.75 * (13 / 40.0)) //pp/n
      ).sum

    val tr = init(sentences, td)

    assertEquals(0.0, tr(AtomCat("<S>"), BadCat), 0.000001)
    assertEquals(0.75 * (5 / 40.0) / sum, tr(AtomCat("<E>"), BadCat), 0.000001)

    assertEquals(0.75 * (6 / 40.0) / sum, tr(s, BadCat), 0.000001)
    assertEquals(0.75 * (13 / 40.0) / sum, tr(n, BadCat), 0.000001)
    assertEquals(0.75 * (2 / 40.0) / sum, tr(s("dcl"), BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * 0.55 * (0.75 * (6 / 40.0)) * (0.75 * (13 / 40.0)) / sum, tr(s / n, BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * 0.55 * (0.75 * (2 / 40.0)) * (0.75 * (13 / 40.0)) / sum, tr(s("dcl") / n, BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (6 / 40.0)) * (0.75 * (13 / 40.0)) / sum, tr(s \ n, BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (2 / 40.0)) * (0.75 * (13 / 40.0)) / sum, tr(s("dcl") \ n, BadCat), 0.000001)

    assertEquals((1 - 0.75) * 0.4 * 0.55 * (0.75 * (13 / 40.0)) * (0.75 * (13 / 40.0)) / sum, tr(n / n, BadCat), 0.000001)
    assertEquals((1 - 0.75) * 0.4 * (1 - 0.55) * (0.75 * (6 / 40.0)) * (0.75 * (6 / 40.0)) / sum, tr(s \ s, BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (2 / 40.0)) * (0.75 * (6 / 40.0)) / sum, tr(s("dcl") \ s, BadCat), 0.000001)

    assertEquals((1 - 0.75) * (1 - 0.4) * 0.55 * ((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (6 / 40.0)) * (0.75 * (13 / 40.0))) * (0.75 * (13 / 40.0)) / sum, tr((s \ n) / n, BadCat), 0.000001)
    assertEquals((1 - 0.75) * (1 - 0.4) * 0.55 * ((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (2 / 40.0)) * (0.75 * (13 / 40.0))) * (0.75 * (13 / 40.0)) / sum, tr((s("dcl") \ n) / n, BadCat), 0.000001)

    assertEquals((1 - 0.75) * 0.4 * 0.55 * ((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (6 / 40.0)) * (0.75 * (13 / 40.0))) * ((1 - 0.75) * (1 - 0.4) * (1 - 0.55) * (0.75 * (6 / 40.0)) * (0.75 * (13 / 40.0))) / sum, tr((s \ n) / (s \ n), BadCat), 0.000001)

    assertEquals((1 - 0.75) * (1 - 0.4) * 0.55 * (0.75 * (6 / 40.0)) * (0.75 * (13 / 40.0)) / sum, tr(s / n, AtomCat("<S>")), 0.000001)
    assertEquals(0.0, tr(s / n, AtomCat("<E>")), 0.000001)
    assertEquals(0.75 * (5 / 40.0) / sum, tr(AtomCat("<E>"), BadCat), 0.000001)
  }

  object BadCat extends Cat {
    def size: Int = ???
    def complexity: Int = ???
    def u(b: Cat): Boolean = ???
  }

}
