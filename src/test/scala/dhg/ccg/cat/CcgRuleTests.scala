package dhg.ccg.cat

import org.junit.Test
import dhg.util.StringUtil._
import org.junit.Assert._

class CcgRuleTests {

  @Test
  def test_CcgRule_apply {
    val x = AtomCat("X")
    val y = AtomCat("Y")
    val z = AtomCat("Z")

    assertEquals(Some(x), FA(x / y, y))
    assertEquals(None, BA(x / y, y))
    assertEquals(None, FC(x / y, y))
    assertEquals(None, BC(x / y, y))
    assertEquals(None, FX(x / y, y))
    assertEquals(None, BX(x / y, y))

    assertEquals(None, FA(y, x \ y))
    assertEquals(Some(x), BA(y, x \ y))
    assertEquals(None, FC(y, x \ y))
    assertEquals(None, BC(y, x \ y))
    assertEquals(None, FX(y, x \ y))
    assertEquals(None, BX(y, x \ y))

    assertEquals(None, FA(x / y, y / z))
    assertEquals(None, BA(x / y, y / z))
    assertEquals(Some(x / z), FC(x / y, y / z))
    assertEquals(None, BC(x / y, y / z))
    assertEquals(None, FX(x / y, y / z))
    assertEquals(None, BX(x / y, y / z))

    assertEquals(None, FA(y \ z, x \ y))
    assertEquals(None, BA(y \ z, x \ y))
    assertEquals(None, FC(y \ z, x \ y))
    assertEquals(Some(x \ z), BC(y \ z, x \ y))
    assertEquals(None, FX(y \ z, x \ y))
    assertEquals(None, BX(y \ z, x \ y))

    assertEquals(None, FA(x / y, y \ z))
    assertEquals(None, BA(x / y, y \ z))
    assertEquals(None, FC(x / y, y \ z))
    assertEquals(None, BC(x / y, y \ z))
    assertEquals(Some(x \ z), FX(x / y, y \ z))
    assertEquals(None, BX(x / y, y \ z))

    assertEquals(None, FA(y / z, x \ y))
    assertEquals(None, BA(y / z, x \ y))
    assertEquals(None, FC(y / z, x \ y))
    assertEquals(None, BC(y / z, x \ y))
    assertEquals(None, FX(y / z, x \ y))
    assertEquals(Some(x / z), BX(y / z, x \ y))

    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    assertEquals(Some(s), FAn2np(s / np, n))
    assertEquals(Some(s), FAn2np(s / np("nb"), n))
    assertEquals(Some(s), FAn2np(s / np, n("nb")))
    assertEquals(Some(s), FAn2np(s / np("nb"), n("nb")))
    assertEquals(None, FAn2np(s / np("nb"), n("ab")))

    assertEquals(None, FAn2np(s / n, np))
    assertEquals(None, FAn2np(s / n("nb"), np))
    assertEquals(None, FAn2np(s / n, np("nb")))
    assertEquals(None, FAn2np(s / n("nb"), np("nb")))
    assertEquals(None, FAn2np(s / n("nb"), np("ab")))

    assertEquals(Some(s), BAn2np(n, s \ np))
    assertEquals(Some(s), BAn2np(n, s \ np("nb")))
    assertEquals(Some(s), BAn2np(n("nb"), s \ np))
    assertEquals(Some(s), BAn2np(n("nb"), s \ np("nb")))
    assertEquals(None, BAn2np(n("ab"), s \ np("nb")))

    assertEquals(None, BAn2np(np, s \ n))
    assertEquals(None, BAn2np(np, s \ n("nb")))
    assertEquals(None, BAn2np(np("nb"), s \ n))
    assertEquals(None, BAn2np(np("nb"), s \ n("nb")))
    assertEquals(None, BAn2np(np("ab"), s \ n("nb")))
  }

  @Test
  def test_CcgRule_infer {
    val a = AtomCat("A")
    val b = AtomCat("B")
    val c = AtomCat("C")
    val d = AtomCat("D")

    val x = AtomCat("X")
    val y = AtomCat("Y")
    val z = AtomCat("Z")

    // Y  __  =>  X
    assertEquals(None, FA.inferRight(x, y))
    assertEquals(Some(x \ y), BA.inferRight(x, y))
    assertEquals(None, FC.inferRight(x, y))
    assertEquals(None, BC.inferRight(x, y))
    assertEquals(None, FX.inferRight(x, y))
    assertEquals(None, BX.inferRight(x, y))

    // __  Y  =>  X
    assertEquals(Some(x / y), FA.inferLeft(x, y))
    assertEquals(None, BA.inferLeft(x, y))
    assertEquals(None, FC.inferLeft(x, y))
    assertEquals(None, BC.inferLeft(x, y))
    assertEquals(None, FX.inferLeft(x, y))
    assertEquals(None, BX.inferLeft(x, y))

    // X/Y  __  =>  X
    assertEquals(Some(y), FA.inferRight(x, x / y))
    assertEquals(Some(x \ (x / y)), BA.inferRight(x, x / y))
    assertEquals(None, FC.inferRight(x, x / y))
    assertEquals(None, BC.inferRight(x, x / y))
    assertEquals(None, FX.inferRight(x, x / y))
    assertEquals(None, BX.inferRight(x, x / y))

    // __  X/Y  =>  X
    assertEquals(Some(x / (x / y)), FA.inferLeft(x, x / y))
    assertEquals(None, BA.inferLeft(x, x / y))
    assertEquals(None, FC.inferLeft(x, x / y))
    assertEquals(None, BC.inferLeft(x, x / y))
    assertEquals(None, FX.inferLeft(x, x / y))
    assertEquals(None, BX.inferLeft(x, x / y))

    // X/Y  __  =>  Z
    assertEquals(None, FA.inferRight(z, x / y))
    assertEquals(Some(z \ (x / y)), BA.inferRight(z, x / y))
    assertEquals(None, FC.inferRight(z, x / y))
    assertEquals(None, BC.inferRight(z, x / y))
    assertEquals(None, FX.inferRight(z, x / y))
    assertEquals(None, BX.inferRight(z, x / y))

    // __  X/Y  =>  Z
    assertEquals(Some(z / (x / y)), FA.inferLeft(z, x / y))
    assertEquals(None, BA.inferLeft(z, x / y))
    assertEquals(None, FC.inferLeft(z, x / y))
    assertEquals(None, BC.inferLeft(z, x / y))
    assertEquals(None, FX.inferLeft(z, x / y))
    assertEquals(None, BX.inferLeft(z, x / y))

    // X\Y  __  =>  X
    assertEquals(None, FA.inferRight(x, x \ y))
    assertEquals(Some(x \ (x \ y)), BA.inferRight(x, x \ y))
    assertEquals(None, FC.inferRight(x, x \ y))
    assertEquals(None, BC.inferRight(x, x \ y))
    assertEquals(None, FX.inferRight(x, x \ y))
    assertEquals(None, BX.inferRight(x, x \ y))

    // __  X\Y  =>  X
    assertEquals(Some(x / (x \ y)), FA.inferLeft(x, x \ y))
    assertEquals(Some(y), BA.inferLeft(x, x \ y))
    assertEquals(None, FC.inferLeft(x, x \ y))
    assertEquals(None, BC.inferLeft(x, x \ y))
    assertEquals(None, FX.inferLeft(x, x \ y))
    assertEquals(None, BX.inferLeft(x, x \ y))

    // X\Y  __  =>  Z
    assertEquals(None, FA.inferRight(z, x \ y))
    assertEquals(Some(z \ (x \ y)), BA.inferRight(z, x \ y))
    assertEquals(None, FC.inferRight(z, x \ y))
    assertEquals(None, BC.inferRight(z, x \ y))
    assertEquals(None, FX.inferRight(z, x \ y))
    assertEquals(None, BX.inferRight(z, x \ y))

    // __  X\Y  =>  Z
    assertEquals(Some(z / (x \ y)), FA.inferLeft(z, x \ y))
    assertEquals(None, BA.inferLeft(z, x \ y))
    assertEquals(None, FC.inferLeft(z, x \ y))
    assertEquals(None, BC.inferLeft(z, x \ y))
    assertEquals(None, FX.inferLeft(z, x \ y))
    assertEquals(None, BX.inferLeft(z, x \ y))

    //

    // A/B  __  =>  A/C
    assertEquals(None, FA.inferRight(a / c, a / b))
    assertEquals(Some((a / c) \ (a / b)), BA.inferRight(a / c, a / b))
    assertEquals(Some(b / c), FC.inferRight(a / c, a / b))
    assertEquals(None, BC.inferRight(a / c, a / b))
    assertEquals(None, FX.inferRight(a / c, a / b))
    assertEquals(None, BX.inferRight(a / c, a / b))

    // __  A\B  =>  A\C
    assertEquals(Some((a \ c) / (a \ b)), FA.inferLeft(a \ c, a \ b))
    assertEquals(None, BA.inferLeft(a \ c, a \ b))
    assertEquals(None, FC.inferLeft(a \ c, a \ b))
    assertEquals(Some(b \ c), BC.inferLeft(a \ c, a \ b))
    assertEquals(None, FX.inferLeft(a \ c, a \ b))
    assertEquals(None, BX.inferLeft(a \ c, a \ b))

    // A/B  __  =>  A\C
    assertEquals(None, FA.inferRight(a \ c, a / b))
    assertEquals(Some((a \ c) \ (a / b)), BA.inferRight(a \ c, a / b))
    assertEquals(None, FC.inferRight(a \ c, a / b))
    assertEquals(None, BC.inferRight(a \ c, a / b))
    assertEquals(Some(b \ c), FX.inferRight(a \ c, a / b))
    assertEquals(None, BX.inferRight(a \ c, a / b))

    // __  A\B  =>  A/C
    assertEquals(Some((a / c) / (a \ b)), FA.inferLeft(a / c, a \ b))
    assertEquals(None, BA.inferLeft(a / c, a \ b))
    assertEquals(None, FC.inferLeft(a / c, a \ b))
    assertEquals(None, BC.inferLeft(a / c, a \ b))
    assertEquals(None, FX.inferLeft(a / c, a \ b))
    assertEquals(Some(b / c), BX.inferLeft(a / c, a \ b))
  }

  @Test
  def test_CcgRule_toString {
    assertEquals("FA", FA.toString)
    assertEquals("FAn2np", FAn2np.toString)
    assertEquals("BA", BA.toString)
    assertEquals("BAn2np", BAn2np.toString)
    assertEquals("FC", FC.toString)
    assertEquals("BC", BC.toString)
    assertEquals("FX", FX.toString)
    assertEquals("BX", BX.toString)
  }

}
