package dhg.ccg.tag.learn

import org.junit.Test
import dhg.util.CollectionUtil._
import dhg.util.StringUtil._
import org.junit.Assert._
import dhg.ccg.tag._

/*
 * TODO: Test with unrestricted tag dictionary
 */
class SupHmmDistributionTests {

  @Test
  def test_tr_unsmoothed() {
    val sentences = Vector(
      "the|D dog|N walks|V",
      "the|D man|N walks|V the|D dog|N",
      "the|D man|N runs|V")
      .map(_.lsplit(" ").map(_.split('|').toTuple2))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set("D"),
      "man" -> Set("N", "V"),
      "walks" -> Set("V")),
      "<S>", "<S>", "<E>", "<E>")

    /* C(t1,t2)
     *    S  D  N  V  E    total
     * S     3           |   3
     * D        4        |   4
     * N           3  1  |   4
     * V     1        2  |   3
     * E
     */

    val tr = new UnsmoothedTransitionDistributioner()(sentences, tagdict)
    assertEquals(0.0, tr("<S>", "<S>"), 1e-5)
    assertEquals(3 / 3.0, tr("D", "<S>"), 1e-5)
    assertEquals(0.0, tr("N", "<S>"), 1e-5)
    assertEquals(0.0, tr("V", "<S>"), 1e-5)
    assertEquals(0.0, tr("default", "<S>"), 1e-5)
    assertEquals(0.0, tr("<E>", "<S>"), 1e-5)

    assertEquals(0.0, tr("<S>", "D"), 1e-5)
    assertEquals(0.0, tr("D", "D"), 1e-5)
    assertEquals(4 / 4.0, tr("N", "D"), 1e-5)
    assertEquals(0.0, tr("V", "D"), 1e-5)
    assertEquals(0.0, tr("default", "D"), 1e-5)
    assertEquals(0.0, tr("<E>", "D"), 1e-5)

    assertEquals(0.0, tr("<S>", "N"), 1e-5)
    assertEquals(0.0, tr("D", "N"), 1e-5)
    assertEquals(0.0, tr("N", "N"), 1e-5)
    assertEquals(3 / 4.0, tr("V", "N"), 1e-5)
    assertEquals(0.0, tr("default", "N"), 1e-5)
    assertEquals(1 / 4.0, tr("<E>", "N"), 1e-5)

    assertEquals(0.0, tr("<S>", "V"), 1e-5)
    assertEquals(1 / 3.0, tr("D", "V"), 1e-5)
    assertEquals(0.0, tr("N", "V"), 1e-5)
    assertEquals(0.0, tr("V", "V"), 1e-5)
    assertEquals(0.0, tr("default", "V"), 1e-5)
    assertEquals(2 / 3.0, tr("<E>", "V"), 1e-5)

    assertEquals(0.0, tr("<S>", "default"), 1e-5)
    assertEquals(0.0, tr("D", "default"), 1e-5)
    assertEquals(0.0, tr("N", "default"), 1e-5)
    assertEquals(0.0, tr("V", "default"), 1e-5)
    assertEquals(0.0, tr("default", "default"), 1e-5)
    assertEquals(0.0, tr("default", "<E>"), 1e-5)

    assertEquals(0.0, tr("<S>", "<E>"), 1e-5)
    assertEquals(0.0, tr("D", "<E>"), 1e-5)
    assertEquals(0.0, tr("N", "<E>"), 1e-5)
    assertEquals(0.0, tr("V", "<E>"), 1e-5)
    assertEquals(0.0, tr("default", "<E>"), 1e-5)
    assertEquals(0.0, tr("<E>", "<E>"), 1e-5)
  }

  @Test
  def test_em_unsmoothed() {
    val sentences = Vector(
      "a|D cat|N chases|V the|D walks|N",
      "the|D dog|N walks|V",
      "the|D man|N walks|V the|D dog|N",
      "the|D man|N runs|V",
      "the|N bird|V walks|D")
      .map(_.lsplit(" ").map(_.split('|').toTuple2))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set("D"),
      "a" -> Set("D"),
      "every" -> Set("D"),
      "some" -> Set("D"),
      "man" -> Set("N", "V"),
      "cat" -> Set("N"),
      "bird" -> Set("N"),
      "fox" -> Set("N"),
      "walks" -> Set("N", "V"),
      "flies" -> Set("N", "V")),
      "<S>", "<S>", "<E>", "<E>")

    /* Words not in TD:
     *   chases
     *   dog
     *   runs
     */

    /* C(t,w)
     * 			D  N  V
     * a		1
     * cat		   1
     * chases	      1
     * the		5  
     * dog		   2  
     * walks	   1  2
     * man		   2
     * runs		      1
     *          -  -  -
     * total	6  6  4
     */

    val em = new UnsmoothedEmissionDistributioner()(sentences, tagdict)
    assertEquals(1.0, em("<S>", "<S>"), 1e-5)
    assertEquals(0.0, em("the", "<S>"), 1e-5)
    assertEquals(0.0, em("a", "<S>"), 1e-5)
    assertEquals(0.0, em("cat", "<S>"), 1e-5)
    assertEquals(0.0, em("man", "<S>"), 1e-5)
    assertEquals(0.0, em("walks", "<S>"), 1e-5)
    assertEquals(0.0, em("dog", "<S>"), 1e-5)
    assertEquals(0.0, em("runs", "<S>"), 1e-5)
    assertEquals(0.0, em("chases", "<S>"), 1e-5)
    assertEquals(0.0, em("default", "<S>"), 1e-5)
    assertEquals(0.0, em("<E>", "<S>"), 1e-5)

    assertEquals(0.0, em("<S>", "D"), 1e-5)
    assertEquals(5 / 6.0, em("the", "D"), 1e-5)
    assertEquals(1 / 6.0, em("a", "D"), 1e-5)
    assertEquals(0.0, em("cat", "D"), 1e-5)
    assertEquals(0.0, em("man", "D"), 1e-5)
    assertEquals(0.0, em("walks", "D"), 1e-5)
    assertEquals(0.0, em("dog", "D"), 1e-5)
    assertEquals(0.0, em("runs", "D"), 1e-5)
    assertEquals(0.0, em("chases", "D"), 1e-5)
    assertEquals(0.0, em("default", "D"), 1e-5)
    assertEquals(0.0, em("<E>", "D"), 1e-5)

    assertEquals(0.0, em("<S>", "N"), 1e-5)
    assertEquals(0.0, em("the", "N"), 1e-5)
    assertEquals(0.0, em("a", "N"), 1e-5)
    assertEquals(1 / 6.0, em("cat", "N"), 1e-5)
    assertEquals(2 / 6.0, em("man", "N"), 1e-5)
    assertEquals(1 / 6.0, em("walks", "N"), 1e-5)
    assertEquals(2 / 6.0, em("dog", "N"), 1e-5)
    assertEquals(0.0, em("runs", "N"), 1e-5)
    assertEquals(0.0, em("chases", "N"), 1e-5)
    assertEquals(0.0, em("default", "N"), 1e-5)
    assertEquals(0.0, em("<E>", "N"), 1e-5)

    assertEquals(0.0, em("<S>", "V"), 1e-5)
    assertEquals(0.0, em("the", "V"), 1e-5)
    assertEquals(0.0, em("a", "V"), 1e-5)
    assertEquals(0.0, em("cat", "V"), 1e-5)
    assertEquals(0.0, em("man", "V"), 1e-5)
    assertEquals(2 / 4.0, em("walks", "V"), 1e-5)
    assertEquals(0.0, em("dog", "V"), 1e-5)
    assertEquals(1 / 4.0, em("runs", "V"), 1e-5)
    assertEquals(1 / 4.0, em("chases", "V"), 1e-5)
    assertEquals(0.0, em("default", "V"), 1e-5)
    assertEquals(0.0, em("<E>", "V"), 1e-5)

    assertEquals(0.0, em("<S>", "default"), 1e-5)
    assertEquals(0.0, em("the", "default"), 1e-5)
    assertEquals(0.0, em("a", "default"), 1e-5)
    assertEquals(0.0, em("cat", "default"), 1e-5)
    assertEquals(0.0, em("man", "default"), 1e-5)
    assertEquals(0.0, em("walks", "default"), 1e-5)
    assertEquals(0.0, em("dog", "default"), 1e-5)
    assertEquals(0.0, em("runs", "default"), 1e-5)
    assertEquals(0.0, em("chases", "default"), 1e-5)
    assertEquals(0.0, em("default", "default"), 1e-5)
    assertEquals(0.0, em("<E>", "default"), 1e-5)

    assertEquals(0.0, em("<S>", "<E>"), 1e-5)
    assertEquals(0.0, em("the", "<E>"), 1e-5)
    assertEquals(0.0, em("a", "<E>"), 1e-5)
    assertEquals(0.0, em("cat", "<E>"), 1e-5)
    assertEquals(0.0, em("man", "<E>"), 1e-5)
    assertEquals(0.0, em("walks", "<E>"), 1e-5)
    assertEquals(0.0, em("dog", "<E>"), 1e-5)
    assertEquals(0.0, em("runs", "<E>"), 1e-5)
    assertEquals(0.0, em("chases", "<E>"), 1e-5)
    assertEquals(0.0, em("default", "<E>"), 1e-5)
    assertEquals(1.0, em("<E>", "<E>"), 1e-5)
  }

  @Test
  def test_tr_addLambda() {
    val sentences = Vector(
      "the|D dog|N walks|V",
      "the|D man|N walks|V the|D dog|N",
      "the|D man|N runs|V")
      .map(_.lsplit(" ").map(_.split('|').toTuple2))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set("D"),
      "man" -> Set("N", "V"),
      "walks" -> Set("V")),
      "<S>", "<S>", "<E>", "<E>")

    /* C(t1,t2)
     *    S  D  N  V  E    total
     * S     3           |   3
     * D        4        |   4
     * N           3  1  |   4
     * V     1        2  |   3
     * E
     */

    val tr = new AddLambdaTransitionDistributioner(0.2)(sentences, tagdict)
    assertEquals(0.0, tr("<S>", "<S>"), 1e-5)
    assertEquals((3 + 0.2) / (3 + 3 * 0.2), tr("D", "<S>"), 1e-5)
    assertEquals((0 + 0.2) / (3 + 3 * 0.2), tr("N", "<S>"), 1e-5)
    assertEquals((0 + 0.2) / (3 + 3 * 0.2), tr("V", "<S>"), 1e-5)
    assertEquals((0 + 0.2) / (3 + 3 * 0.2), tr("default", "<S>"), 1e-5)
    assertEquals(0.0, tr("<E>", "<S>"), 1e-5)

    assertEquals(0.0, tr("<S>", "D"), 1e-5)
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tr("D", "D"), 1e-5)
    assertEquals((4 + 0.2) / (4 + 4 * 0.2), tr("N", "D"), 1e-5)
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tr("V", "D"), 1e-5)
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tr("default", "D"), 1e-5)
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tr("<E>", "D"), 1e-5)

    assertEquals(0.0, tr("<S>", "N"), 1e-5)
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tr("D", "N"), 1e-5)
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tr("N", "N"), 1e-5)
    assertEquals((3 + 0.2) / (4 + 4 * 0.2), tr("V", "N"), 1e-5)
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tr("default", "N"), 1e-5)
    assertEquals((1 + 0.2) / (4 + 4 * 0.2), tr("<E>", "N"), 1e-5)

    assertEquals(0.0, tr("<S>", "V"), 1e-5)
    assertEquals((1 + 0.2) / (3 + 4 * 0.2), tr("D", "V"), 1e-5)
    assertEquals((0 + 0.2) / (3 + 4 * 0.2), tr("N", "V"), 1e-5)
    assertEquals((0 + 0.2) / (3 + 4 * 0.2), tr("V", "V"), 1e-5)
    assertEquals((0 + 0.2) / (3 + 4 * 0.2), tr("default", "V"), 1e-5)
    assertEquals((2 + 0.2) / (3 + 4 * 0.2), tr("<E>", "V"), 1e-5)

    assertEquals(0.0, tr("<S>", "default"), 1e-5)
    assertEquals((0 + 0.2) / (0 + 4 * 0.2), tr("D", "default"), 1e-5)
    assertEquals((0 + 0.2) / (0 + 4 * 0.2), tr("N", "default"), 1e-5)
    assertEquals((0 + 0.2) / (0 + 4 * 0.2), tr("V", "default"), 1e-5)
    assertEquals((0 + 0.2) / (0 + 4 * 0.2), tr("default", "default"), 1e-5)
    assertEquals((0 + 0.2) / (0 + 4 * 0.2), tr("<E>", "default"), 1e-5)

    assertEquals(0.0, tr("<S>", "<E>"), 1e-5)
    assertEquals(0.0, tr("D", "<E>"), 1e-5)
    assertEquals(0.0, tr("N", "<E>"), 1e-5)
    assertEquals(0.0, tr("V", "<E>"), 1e-5)
    assertEquals(0.0, tr("default", "<E>"), 1e-5)
    assertEquals(0.0, tr("<E>", "<E>"), 1e-5)
  }

  @Test
  def test_em_addLambda() {
    val sentences = Vector(
      "a|D cat|N chases|V the|D walks|N",
      "the|D dog|N walks|V",
      "the|D man|N walks|V the|D dog|N",
      "the|D man|N runs|V",
      "the|N bird|V walks|D")
      .map(_.lsplit(" ").map(_.split('|').toTuple2))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set("D"),
      "a" -> Set("D"),
      "big" -> Set("A"),
      "good" -> Set("A", "N"),
      "every" -> Set("D"),
      "some" -> Set("D"),
      "man" -> Set("N", "V"),
      "cat" -> Set("N"),
      "bird" -> Set("N"),
      "fox" -> Set("N"),
      "walks" -> Set("N", "V"),
      "flies" -> Set("N", "V")),
      "<S>", "<S>", "<E>", "<E>")

    /* Words not in TD:
     *   chases
     *   dog
     *   runs
     */

    /* C(t,w)
     * 			D  N  V
     * a		1
     * cat		   1
     * chases	      1
     * the		5  
     * dog		   2  
     * walks	   1  2
     * man		   2
     * runs		      1
     *          -  -  -
     * total	6  6  4
     */

    val em = new AddLambdaEmissionDistributioner(0.2)(sentences, tagdict)
    assertEquals(1.0, em("<S>", "<S>"), 1e-5)
    assertEquals(0.0, em("a", "<S>"), 1e-5)
    assertEquals(0.0, em("bird", "<S>"), 1e-5)
    assertEquals(0.0, em("cat", "<S>"), 1e-5)
    assertEquals(0.0, em("chases", "<S>"), 1e-5)
    assertEquals(0.0, em("dog", "<S>"), 1e-5)
    assertEquals(0.0, em("every", "<S>"), 1e-5)
    assertEquals(0.0, em("flies", "<S>"), 1e-5)
    assertEquals(0.0, em("fox", "<S>"), 1e-5)
    assertEquals(0.0, em("man", "<S>"), 1e-5)
    assertEquals(0.0, em("runs", "<S>"), 1e-5)
    assertEquals(0.0, em("some", "<S>"), 1e-5)
    assertEquals(0.0, em("the", "<S>"), 1e-5)
    assertEquals(0.0, em("walks", "<S>"), 1e-5)
    assertEquals(0.0, em("big", "<S>"), 1e-5)
    assertEquals(0.0, em("good", "<S>"), 1e-5)
    assertEquals(0.0, em("default", "<S>"), 1e-5)
    assertEquals(0.0, em("<E>", "<S>"), 1e-5)

    assertEquals(0.0, em("<S>", "D"), 1e-5)
    assertEquals((1 + 0.2) / (6 + 7 * 0.2), em("a", "D"), 1e-5)
    assertEquals(0.0, em("bird", "D"), 1e-5)
    assertEquals(0.0, em("cat", "D"), 1e-5)
    assertEquals((0 + 0.2) / (6 + 7 * 0.2), em("chases", "D"), 1e-5)
    assertEquals((0 + 0.2) / (6 + 7 * 0.2), em("dog", "D"), 1e-5)
    assertEquals((0 + 0.2) / (6 + 7 * 0.2), em("every", "D"), 1e-5)
    assertEquals(0.0, em("flies", "D"), 1e-5)
    assertEquals(0.0, em("fox", "D"), 1e-5)
    assertEquals(0.0, em("man", "D"), 1e-5)
    assertEquals((0 + 0.2) / (6 + 7 * 0.2), em("runs", "D"), 1e-5)
    assertEquals((0 + 0.2) / (6 + 7 * 0.2), em("some", "D"), 1e-5)
    assertEquals((5 + 0.2) / (6 + 7 * 0.2), em("the", "D"), 1e-5)
    assertEquals(0.0, em("walks", "D"), 1e-5)
    assertEquals(0.0, em("big", "D"), 1e-5)
    assertEquals(0.0, em("good", "D"), 1e-5)
    assertEquals((0 + 0.2) / (6 + 7 * 0.2), em("default", "D"), 1e-5)
    assertEquals(0.0, em("<E>", "D"), 1e-5)

    assertEquals(0.0, em("<S>", "N"), 1e-5)
    assertEquals(0.0, em("a", "N"), 1e-5)
    assertEquals((0 + 0.2) / (6 + 10 * 0.2), em("bird", "N"), 1e-5)
    assertEquals((1 + 0.2) / (6 + 10 * 0.2), em("cat", "N"), 1e-5)
    assertEquals((0 + 0.2) / (6 + 10 * 0.2), em("chases", "N"), 1e-5)
    assertEquals((2 + 0.2) / (6 + 10 * 0.2), em("dog", "N"), 1e-5)
    assertEquals(0.0, em("every", "N"), 1e-5)
    assertEquals((0 + 0.2) / (6 + 10 * 0.2), em("flies", "N"), 1e-5)
    assertEquals((0 + 0.2) / (6 + 10 * 0.2), em("fox", "N"), 1e-5)
    assertEquals((2 + 0.2) / (6 + 10 * 0.2), em("man", "N"), 1e-5)
    assertEquals((0 + 0.2) / (6 + 10 * 0.2), em("runs", "N"), 1e-5)
    assertEquals(0.0, em("some", "N"), 1e-5)
    assertEquals(0.0, em("the", "N"), 1e-5)
    assertEquals((1 + 0.2) / (6 + 10 * 0.2), em("walks", "N"), 1e-5)
    assertEquals(0.0, em("big", "N"), 1e-5)
    assertEquals((0 + 0.2) / (6 + 10 * 0.2), em("good", "N"), 1e-5)
    assertEquals((0 + 0.2) / (6 + 10 * 0.2), em("default", "N"), 1e-5)
    assertEquals(0.0, em("<E>", "N"), 1e-5)

    assertEquals(0.0, em("<S>", "V"), 1e-5)
    assertEquals(0.0, em("a", "V"), 1e-5)
    assertEquals(0.0, em("bird", "V"), 1e-5)
    assertEquals(0.0, em("cat", "V"), 1e-5)
    assertEquals((1 + 0.2) / (4 + 6 * 0.2), em("chases", "V"), 1e-5)
    assertEquals((0 + 0.2) / (4 + 6 * 0.2), em("dog", "V"), 1e-5)
    assertEquals(0.0, em("every", "V"), 1e-5)
    assertEquals((0 + 0.2) / (4 + 6 * 0.2), em("flies", "V"), 1e-5)
    assertEquals(0.0, em("fox", "V"), 1e-5)
    assertEquals((0 + 0.2) / (4 + 6 * 0.2), em("man", "V"), 1e-5)
    assertEquals((1 + 0.2) / (4 + 6 * 0.2), em("runs", "V"), 1e-5)
    assertEquals(0.0, em("some", "V"), 1e-5)
    assertEquals(0.0, em("the", "V"), 1e-5)
    assertEquals((2 + 0.2) / (4 + 6 * 0.2), em("walks", "V"), 1e-5)
    assertEquals(0.0, em("big", "V"), 1e-5)
    assertEquals(0.0, em("good", "V"), 1e-5)
    assertEquals((0 + 0.2) / (4 + 6 * 0.2), em("default", "V"), 1e-5)
    assertEquals(0.0, em("<E>", "V"), 1e-5)

    assertEquals(0.0, em("<S>", "A"), 1e-5)
    assertEquals(0.0, em("a", "A"), 1e-5)
    assertEquals(0.0, em("bird", "A"), 1e-5)
    assertEquals(0.0, em("cat", "A"), 1e-5)
    assertEquals((0 + 0.2) / (0 + 5 * 0.2), em("chases", "A"), 1e-5)
    assertEquals((0 + 0.2) / (0 + 5 * 0.2), em("dog", "A"), 1e-5)
    assertEquals(0.0, em("every", "A"), 1e-5)
    assertEquals(0.0, em("flies", "A"), 1e-5)
    assertEquals(0.0, em("fox", "A"), 1e-5)
    assertEquals(0.0, em("man", "A"), 1e-5)
    assertEquals((0 + 0.2) / (0 + 5 * 0.2), em("runs", "A"), 1e-5)
    assertEquals(0.0, em("some", "A"), 1e-5)
    assertEquals(0.0, em("the", "A"), 1e-5)
    assertEquals(0.0, em("walks", "A"), 1e-5)
    assertEquals((0 + 0.2) / (0 + 5 * 0.2), em("big", "A"), 1e-5)
    assertEquals((0 + 0.2) / (0 + 5 * 0.2), em("good", "A"), 1e-5)
    assertEquals((0 + 0.2) / (0 + 5 * 0.2), em("default", "A"), 1e-5)
    assertEquals(0.0, em("<E>", "A"), 1e-5)

    assertEquals(0.0, em("<S>", "default"), 1e-5)
    assertEquals(0.0, em("a", "default"), 1e-5)
    assertEquals(0.0, em("bird", "default"), 1e-5)
    assertEquals(0.0, em("cat", "default"), 1e-5)
    assertEquals((0 + 0.2) / (0 + 3 * 0.2), em("chases", "default"), 1e-5)
    assertEquals((0 + 0.2) / (0 + 3 * 0.2), em("dog", "default"), 1e-5)
    assertEquals(0.0, em("every", "default"), 1e-5)
    assertEquals(0.0, em("flies", "default"), 1e-5)
    assertEquals(0.0, em("fox", "default"), 1e-5)
    assertEquals(0.0, em("man", "default"), 1e-5)
    assertEquals((0 + 0.2) / (0 + 3 * 0.2), em("runs", "default"), 1e-5)
    assertEquals(0.0, em("some", "default"), 1e-5)
    assertEquals(0.0, em("the", "default"), 1e-5)
    assertEquals(0.0, em("walks", "default"), 1e-5)
    assertEquals(0.0, em("big", "default"), 1e-5)
    assertEquals(0.0, em("good", "default"), 1e-5)
    assertEquals((0 + 0.2) / (0 + 3 * 0.2), em("default", "default"), 1e-5)
    assertEquals(0.0, em("<E>", "default"), 1e-5)

    assertEquals(0.0, em("<S>", "<E>"), 1e-5)
    assertEquals(0.0, em("a", "<E>"), 1e-5)
    assertEquals(0.0, em("bird", "<E>"), 1e-5)
    assertEquals(0.0, em("cat", "<E>"), 1e-5)
    assertEquals(0.0, em("chases", "<E>"), 1e-5)
    assertEquals(0.0, em("dog", "<E>"), 1e-5)
    assertEquals(0.0, em("every", "<E>"), 1e-5)
    assertEquals(0.0, em("flies", "<E>"), 1e-5)
    assertEquals(0.0, em("fox", "<E>"), 1e-5)
    assertEquals(0.0, em("man", "<E>"), 1e-5)
    assertEquals(0.0, em("runs", "<E>"), 1e-5)
    assertEquals(0.0, em("some", "<E>"), 1e-5)
    assertEquals(0.0, em("the", "<E>"), 1e-5)
    assertEquals(0.0, em("walks", "<E>"), 1e-5)
    assertEquals(0.0, em("big", "<E>"), 1e-5)
    assertEquals(0.0, em("good", "<E>"), 1e-5)
    assertEquals(0.0, em("default", "<E>"), 1e-5)
    assertEquals(1.0, em("<E>", "<E>"), 1e-5)
  }

}
