package dhg.ccg.cat

import org.junit.Test
import dhg.util.StringUtil._
import org.junit.Assert._

class CatTests {

  @Test
  def test_base {
    assertEquals(AtomCat("NP", None), AtomCat("NP", Some("nb")).base)
    assertEquals(AtomCat("NP", None), AtomCat("NP", None).base)
  }

  @Test
  def test_operators {
    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    assertEquals(AtomCat("NP", Some("nb")), np("nb"))

    assertEquals(BCat(AtomCat("S", None), AtomCat("NP", None)), s \ np)
    assertEquals(FCat(AtomCat("S", None), AtomCat("NP", None)), s / np)
    assertEquals(FCat(AtomCat("NP", Some("nb")), AtomCat("N", None)), np("nb") / n)
    assertEquals(BCat(AtomCat("NP", None), AtomCat("NP", Some("nb"))), np \ np("nb"))

    assertEquals(BCat(FCat(AtomCat("S", None), AtomCat("NP", Some("nb"))), AtomCat("S", None)), (s / np("nb")) \ s)
    assertEquals(FCat(BCat(AtomCat("S", None), AtomCat("NP", None)), AtomCat("NP", Some("nb"))), (s \ np) / np("nb"))
    assertEquals(FCat(BCat(AtomCat("S", Some("dcl")), AtomCat("NP", None)), AtomCat("NP", None)), (s("dcl") \ np) / np)
    assertEquals(FCat(BCat(BCat(AtomCat("S", None), AtomCat("NP", None)), BCat(AtomCat("S", None), AtomCat("NP", None))), AtomCat("NP", None)), ((s \ np) \ (s \ np)) / np)
  }

  @Test
  def test_unapply {
    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    assertEquals(AtomCat("NP", Some("nb")), np("nb"))

    val (a / b) = s / np
    assertEquals(s, a)
    assertEquals(np, b)

    val (c \ d) = (s / np("nb")) \ np
    assertEquals(s / np("nb"), c)
    assertEquals(np, d)

    val (e \/ f) = s / np("nb")
    assertEquals(s, e)
    assertEquals(np("nb"), f)

    val (g \/ h) = s("dcl") \ (s / np)
    assertEquals(s("dcl"), g)
    assertEquals((s / np), h)

    assertTrue(np match { case (a / b) => false; case _ => true })
    assertTrue(s \ np match { case (a / b) => false; case _ => true })
    assertTrue(np match { case (a \ b) => false; case _ => true })
    assertTrue(s / np match { case (a \ b) => false; case _ => true })
    assertTrue(np match { case (a \/ b) => false; case _ => true })
  }

  @Test
  def test_unify {
    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    assertTrue(s u s)
    assertTrue(s("dcl") u s)
    assertTrue(s u s("dcl"))
    assertTrue(s("dcl") u s("dcl"))

    assertFalse(s u n)
    assertFalse(s("dcl") u n)
    assertFalse(s u n("dcl"))
    assertFalse(s("dcl") u n("dcl"))

    assertTrue((s \ np) u (s \ np))
    assertTrue((s("dcl") \ np) u (s("dcl") \ np))
    assertTrue((s("dcl") \ np) u (s \ np))
    assertTrue((s \ np) u (s("dcl") \ np))
    assertTrue((s \ np("nb")) u (s("dcl") \ np))
    assertTrue((s \ np) u (s("dcl") \ np("nb")))

    assertFalse((s \ np) u (s / np))
    assertFalse((s("dcl") \ np) u (s("dcl") / np))
    assertFalse((s("dcl") \ np) u (s / np))
    assertFalse((s \ np) u (s("dcl") / np))
    assertFalse((s \ np("nb")) u (s("dcl") / np))
    assertFalse((s \ np) u (s("dcl") / np("nb")))

    assertFalse((s \ np) u (n \ np))
    assertFalse((s("dcl") \ np) u (n("dcl") \ np))
    assertFalse((s("dcl") \ np) u (n \ np))
    assertFalse((s \ np) u (n("dcl") \ np))
    assertFalse((s \ np("nb")) u (n("dcl") \ np))
    assertFalse((s \ np) u (n("dcl") \ np("nb")))

    assertFalse((s \ np) u (s \ s))
    assertFalse((s("dcl") \ np) u (s("dcl") \ s))
    assertFalse((s("dcl") \ np) u (s \ s))
    assertFalse((s \ np) u (s("dcl") \ s))
    assertFalse((s \ np("nb")) u (s("dcl") \ s))
    assertFalse((s \ np) u (s("dcl") \ s("nb")))

    assertTrue((s / np) u (s / np))
    assertTrue((s("dcl") / np) u (s("dcl") / np))
    assertTrue((s("dcl") / np) u (s / np))
    assertTrue((s / np) u (s("dcl") / np))
    assertTrue((s / np("nb")) u (s("dcl") / np))
    assertTrue((s / np) u (s("dcl") / np("nb")))

    assertFalse((s / np) u (s \ np))
    assertFalse((s("dcl") / np) u (s("dcl") \ np))
    assertFalse((s("dcl") / np) u (s \ np))
    assertFalse((s / np) u (s("dcl") \ np))
    assertFalse((s / np("nb")) u (s("dcl") \ np))
    assertFalse((s / np) u (s("dcl") \ np("nb")))

    assertFalse((s / np) u (n / np))
    assertFalse((s("dcl") / np) u (n("dcl") / np))
    assertFalse((s("dcl") / np) u (n / np))
    assertFalse((s / np) u (n("dcl") / np))
    assertFalse((s / np("nb")) u (n("dcl") / np))
    assertFalse((s / np) u (n("dcl") / np("nb")))

    assertFalse((s / np) u (s / s))
    assertFalse((s("dcl") / np) u (s("dcl") / s))
    assertFalse((s("dcl") / np) u (s / s))
    assertFalse((s / np) u (s("dcl") / s))
    assertFalse((s / np("nb")) u (s("dcl") / s))
    assertFalse((s / np) u (s("dcl") / s("nb")))

    assertTrue((((s("dcl") \ np) \ (s \ np)) / np) u (((s("dcl") \ np("ab")) \ (s \ np("nb"))) / np))
    assertFalse((((s("dcl") \ np) \ (s \ np)) / np) u (((s("int") \ np("ab")) \ (s \ np("nb"))) / np))
  }

  @Test
  def test_toString {
    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    assertEquals("""(((S[dcl]\NP)\(S\NP[nb]))/NP)""", (((s("dcl") \ np) \ (s \ np("nb"))) / np).toString)
  }

  @Test
  def test_size {
    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    assertEquals(1, (n).size)
    assertEquals(1, (np).size)
    assertEquals(1, (np("nb")).size)
    assertEquals(2, (s \ np).size)
    assertEquals(2, (s / np).size)
    assertEquals(2, (np / n).size)
    assertEquals(2, (np \ np).size)
    assertEquals(3, ((s / np) \ s).size)
    assertEquals(3, ((s \ np) / np).size)
    assertEquals(3, ((s("dcl") \ np) / np).size)
    assertEquals(4, (((s \ np) \ (s \ np))).size)
    assertEquals(4, ((((s \ np) \ s) \ np)).size)
    assertEquals(5, (((s \ np) \ (s \ np)) / np).size)
  }

  @Test
  def test_complexity {
    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    assertEquals(1, (n).complexity)
    assertEquals(1, (np).complexity)
    assertEquals(1, (np("nb")).complexity)
    assertEquals(3, (s \ np).complexity)
    assertEquals(3, (s / np).complexity)
    assertEquals(3, (np / n).complexity)
    assertEquals(3, (np \ np).complexity)
    assertEquals(5, ((s / np) \ s).complexity)
    assertEquals(5, ((s \ np) / np).complexity)
    assertEquals(5, ((s("dcl") \ np) / np).complexity)
    assertEquals(7, (((s \ np) \ (s \ np))).complexity)
    assertEquals(7, ((((s \ np) \ s) \ np)).complexity)
    assertEquals(9, (((s \ np) \ (s \ np)) / np).complexity)
  }

  @Test
  def test_canCombine {
    val cc = new SimpleCatCanCombine(Vector(
      FA, FAn2np, BA, BAn2np, FC, BC, /*FX,*/ BX),
      AtomCat("<S>"), AtomCat("<E>"))

    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    assertTrue(cc(np, s \ np))
    assertTrue(cc(s / np, np / n))
    assertTrue(cc((s \ np) / np, ((s \ np) \ (s \ np))))
    assertFalse(cc(s / np, np \ np))
    assertTrue(cc((s / np) \ s, np / n))
    assertTrue(cc(np, (s \ np) / np))
    assertTrue(cc(np("nb"), s \ np))
    assertTrue(cc(n, s \ np))
    assertFalse(cc(np / n, np))
    assertTrue(cc(n, (s("dcl") \ np) / np))

    assertTrue(cc(AtomCat("<S>"), s / np))
    assertFalse(cc(AtomCat("<S>"), s \ np))
    assertFalse(cc(s / np, AtomCat("<E>")))
    assertTrue(cc(s \ np, AtomCat("<E>")))
  }

}
