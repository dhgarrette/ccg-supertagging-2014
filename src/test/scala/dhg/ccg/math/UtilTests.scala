package dhg.ccg.math

import org.junit.Test
import dhg.util.TestUtil._
import dhg.util.StringUtil._
import math.{ log, exp, abs, pow }
import org.junit.Assert._
import Double.NaN
import scala.util.Random
import dhg.ccg.math.Util._
import org.apache.commons.math3.random.{ MersenneTwister, RandomGenerator }
import dhg.ccg.test.TestUtil.DoubleIteratorRandomGenerator

class UtilTests {

  @Test
  def test_choose {

    def r(d: Double) = DoubleIteratorRandomGenerator(Iterator(d))

    //  0   1   2   3   4   5   6   7   8   9  10
    //  0   2   4   6   8  10  12  14  16  18  20
    //  0       4           6               8   2
    // 0.0 --- 0.2 ------- 0.5 ----------- 0.9 1.0   

    assertEquals(0, choose(Array[Double](4), 1, r(0.1)))

    assertEquals(0, choose(Array[Double](4, 6, 8, 2), 4, r(0.1)))
    assertEquals(1, choose(Array[Double](4, 6, 8, 2), 4, r(0.4)))
    assertEquals(2, choose(Array[Double](4, 6, 8, 2), 4, r(0.6)))
    assertEquals(3, choose(Array[Double](4, 6, 8, 2), 4, r(0.95)))
    assertExceptionMsg("No value chosen! .*".r)(choose(Array[Double](4, 6, 8, 2), 4, r(1.2)))
    assertExceptionMsg("No value chosen! .*".r)(choose(Array[Double](4, 6, 8, 2), 4, r(1.0)))

    assertEquals(0, choose(Array[Double](4, 6, 8, 2, arb, arb), 4, r(0.1)))
    assertEquals(1, choose(Array[Double](4, 6, 8, 2, arb, arb), 4, r(0.4)))
    assertEquals(2, choose(Array[Double](4, 6, 8, 2, arb, arb), 4, r(0.6)))
    assertEquals(3, choose(Array[Double](4, 6, 8, 2, arb, arb), 4, r(0.95)))
    assertExceptionMsg("No value chosen! .*".r)(choose(Array[Double](4, 6, 8, 2, arb, arb), 4, r(1.2)))
    assertExceptionMsg("No value chosen! .*".r)(choose(Array[Double](4, 6, 8, 2, arb, arb), 4, r(1.0)))
  }

  @Test
  def test_logChoose {

    def r(d: Double) = DoubleIteratorRandomGenerator(Iterator(d))

    //  0   1   2   3   4   5   6   7   8   9  10
    //  0   2   4   6   8  10  12  14  16  18  20
    //  0       4           6               8   2
    // 0.0 --- 0.2 ------- 0.5 ----------- 0.9 1.0   

    assertEquals(0, logChoose(Array[Double](4).map(log), Array(0), 1, r(0.1)))
    assertEquals(0, logChoose(Array[Double](4).map(log), Array(0, 3, 4, 5), 1, r(0.1)))
    assertEquals(1, logChoose(Array[Double](4, 6, 8).map(log), Array(1), 1, r(0.1)))
    assertEquals(1, logChoose(Array[Double](4, 6, 8).map(log), Array(1), 1, r(0.1)))
    assertEquals(1, logChoose(Array[Double](4, 6, 8).map(log), Array(1, 3, 4, 5), 1, r(0.1)))

    assertEquals(0, logChoose(Array[Double](4, 6, 8, 2).map(log), Array(0, 1, 2, 3), 4, r(0.1)))
    assertEquals(1, logChoose(Array[Double](4, 6, 8, 2).map(log), Array(0, 1, 2, 3), 4, r(0.4)))
    assertEquals(2, logChoose(Array[Double](4, 6, 8, 2).map(log), Array(0, 1, 2, 3), 4, r(0.6)))
    assertEquals(3, logChoose(Array[Double](4, 6, 8, 2).map(log), Array(0, 1, 2, 3), 4, r(0.95)))
    assertExceptionMsg("No value chosen! .*".r)(logChoose(Array[Double](4, 6, 8, 2).map(log), Array(0, 1, 2, 3), 4, r(1.0)))

    assertEquals(2, logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2).map(log), Array(2, 4, 5, 7), 4, r(0.1)))
    assertEquals(4, logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2).map(log), Array(2, 4, 5, 7), 4, r(0.4)))
    assertEquals(5, logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2).map(log), Array(2, 4, 5, 7), 4, r(0.6)))
    assertEquals(7, logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2).map(log), Array(2, 4, 5, 7), 4, r(0.95)))
    assertExceptionMsg("No value chosen! .*".r)(logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2).map(log), Array(2, 4, 5, 7), 4, r(1.0)))

    assertEquals(0, logChoose(Array(4, arb, 6, 8, arb, 2).map(log), Array(0, 2, 3, 5), 4, r(0.1)))
    assertEquals(2, logChoose(Array(4, arb, 6, 8, arb, 2).map(log), Array(0, 2, 3, 5), 4, r(0.4)))
    assertEquals(3, logChoose(Array(4, arb, 6, 8, arb, 2).map(log), Array(0, 2, 3, 5), 4, r(0.6)))
    assertEquals(5, logChoose(Array(4, arb, 6, 8, arb, 2).map(log), Array(0, 2, 3, 5), 4, r(0.95)))
    assertExceptionMsg("No value chosen! .*".r)(logChoose(Array(4, arb, 6, 8, arb, 2).map(log), Array(0, 2, 3, 5), 4, r(1.0)))

    assertEquals(0, logChoose(Array(4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(0, 2, 3, 5), 4, r(0.1)))
    assertEquals(2, logChoose(Array(4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(0, 2, 3, 5), 4, r(0.4)))
    assertEquals(3, logChoose(Array(4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(0, 2, 3, 5), 4, r(0.6)))
    assertEquals(5, logChoose(Array(4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(0, 2, 3, 5), 4, r(0.95)))
    assertExceptionMsg("No value chosen! .*".r)(logChoose(Array(4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(0, 2, 3, 5), 4, r(1.0)))

    assertEquals(2, logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(2, 4, 5, 7), 4, r(0.1)))
    assertEquals(4, logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(2, 4, 5, 7), 4, r(0.4)))
    assertEquals(5, logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(2, 4, 5, 7), 4, r(0.6)))
    assertEquals(7, logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(2, 4, 5, 7), 4, r(0.95)))
    assertExceptionMsg("No value chosen! .*".r)(logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(2, 4, 5, 7), 4, r(1.0)))

    assertEquals(0, logChoose(Array[Double](4, 6, 8, 2).map(log), Array(0, 1, 2, 3, 8, 9), 4, r(0.1)))
    assertEquals(1, logChoose(Array[Double](4, 6, 8, 2).map(log), Array(0, 1, 2, 3, 8, 9), 4, r(0.4)))
    assertEquals(2, logChoose(Array[Double](4, 6, 8, 2).map(log), Array(0, 1, 2, 3, 8, 9), 4, r(0.6)))
    assertEquals(3, logChoose(Array[Double](4, 6, 8, 2).map(log), Array(0, 1, 2, 3, 8, 9), 4, r(0.95)))
    assertExceptionMsg("No value chosen! .*".r)(logChoose(Array[Double](4, 6, 8, 2).map(log), Array(0, 1, 2, 3, 8, 9), 4, r(1.0)))

    assertEquals(2, logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2).map(log), Array(2, 4, 5, 7, 8, 9), 4, r(0.1)))
    assertEquals(4, logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2).map(log), Array(2, 4, 5, 7, 8, 9), 4, r(0.4)))
    assertEquals(5, logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2).map(log), Array(2, 4, 5, 7, 8, 9), 4, r(0.6)))
    assertEquals(7, logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2).map(log), Array(2, 4, 5, 7, 8, 9), 4, r(0.95)))
    assertExceptionMsg("No value chosen! .*".r)(logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2).map(log), Array(2, 4, 5, 7, 8, 9), 4, r(1.0)))

    assertEquals(0, logChoose(Array(4, arb, 6, 8, arb, 2).map(log), Array(0, 2, 3, 5, 8, 9), 4, r(0.1)))
    assertEquals(2, logChoose(Array(4, arb, 6, 8, arb, 2).map(log), Array(0, 2, 3, 5, 8, 9), 4, r(0.4)))
    assertEquals(3, logChoose(Array(4, arb, 6, 8, arb, 2).map(log), Array(0, 2, 3, 5, 8, 9), 4, r(0.6)))
    assertEquals(5, logChoose(Array(4, arb, 6, 8, arb, 2).map(log), Array(0, 2, 3, 5, 8, 9), 4, r(0.95)))
    assertExceptionMsg("No value chosen! .*".r)(logChoose(Array(4, arb, 6, 8, arb, 2).map(log), Array(0, 2, 3, 5, 8, 9), 4, r(1.0)))

    assertEquals(0, logChoose(Array(4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(0, 2, 3, 5, 8, 9), 4, r(0.1)))
    assertEquals(2, logChoose(Array(4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(0, 2, 3, 5, 8, 9), 4, r(0.4)))
    assertEquals(3, logChoose(Array(4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(0, 2, 3, 5, 8, 9), 4, r(0.6)))
    assertEquals(5, logChoose(Array(4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(0, 2, 3, 5, 8, 9), 4, r(0.95)))
    assertExceptionMsg("No value chosen! .*".r)(logChoose(Array(4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(0, 2, 3, 5, 8, 9), 4, r(1.0)))

    assertEquals(2, logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(2, 4, 5, 7, 8, 9), 4, r(0.1)))
    assertEquals(4, logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(2, 4, 5, 7, 8, 9), 4, r(0.4)))
    assertEquals(5, logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(2, 4, 5, 7, 8, 9), 4, r(0.6)))
    assertEquals(7, logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(2, 4, 5, 7, 8, 9), 4, r(0.95)))
    assertExceptionMsg("No value chosen! .*".r)(logChoose(Array(arb, arb, 4, arb, 6, 8, arb, 2, arb, arb).map(log), Array(2, 4, 5, 7), 4, r(1.0)))

  }

  @Test
  def test_convertToLogDirichletDraw {
    val rand = new MersenneTwister(0)
    def check(arr: Array[Double]): Unit = {
      assertEquals(1.0, exp(logSum(arr)), 1e-9)
      for (v <- arr) { assertFalse(v.isNegInfinity); assertTrue(v < 0.0) }
    }
    { val a = Array(4.0, 2.0, 0.1, 10.0); convertToLogDirichletDraw(a, 4, rand); check(a) }
  }

  @Test
  def test_convertActiveToLogDirichletDraw {
    val rand = new MersenneTwister(0)
    def check(arr: Array[Double], active: Array[Int]): Unit = {
      assertEquals(1.0, exp(logSum(arr.zipWithIndex.filter(x => active.contains(x._2)).map(_._1))), 1e-9)
      for (i <- active) { assertFalse(arr(i).isNegInfinity); assertTrue(arr(i) < 0.0) }
    }
    { val a = Array(arb, arb, 4.0, arb, 2.0, 0.1, arb, 10.0, arb); val active = Array(2, 4, 5, 7); convertActiveToLogDirichletDraw(a, active, 4, rand); check(a, active) }
  }

  @Test
  def test_activeLogSum {
    //    assertEquals(log(0), activeLogSum(Array[Double](), Array(), 0), 1e-9)
    //    assertEquals(log(0), activeLogSum(Array[Double](arb, arb, arb), Array(), 0), 1e-9)
    //    assertEquals(log(3), activeLogSum(Array[Double](3).map(log), Array(0), 1), 1e-9)
    assertEquals(log(7), activeLogSum(Array[Double](3, 4).map(log), Array(0, 1), 2), 1e-9)
    assertEquals(log(15), activeLogSum(Array[Double](3, 4, 7, 1).map(log), Array(0, 1, 2, 3), 4), 1e-9)
    assertEquals(log(15), activeLogSum(Array[Double](3, 4, 7, 1, 6, 8, 5, 2).map(log), Array(0, 1, 2, 3), 4), 1e-9)
    assertEquals(log(4 + 1 + 6 + 5), activeLogSum(Array[Double](6, 4, 7, 1, 6, 8, 5, 2).map(log), Array(1, 3, 4, 6), 4), 1e-9)
    assertEquals(log(4 + 1 + 6 + 5), activeLogSum(Array[Double](6, 4, 7, 1, 6, 8, 5, 2).map(log), Array(1, 3, 4, 6, 7, 8), 4), 1e-9)
  }

  @Test
  def test_activeMax {
    //    assertEquals(log(0), activeLogSum(Array[Double](), Array(), 0), 1e-9)
    //    assertEquals(log(0), activeLogSum(Array[Double](arb, arb, arb), Array(), 0), 1e-9)
    assertEquals(3, activeMax(Array(3), Array(0), 1), 1e-9)
    assertEquals(3, activeMax(Array(3), Array(0, 5, 6, 7), 1), 1e-9)
    assertEquals(5, activeMax(Array(3, 5, 7), Array(1, 5, 6, 7), 1), 1e-9)
    assertEquals(4, activeMax(Array(3, 4), Array(0, 1), 2), 1e-9)
    assertEquals(7, activeMax(Array(3, 4, 7, 1), Array(0, 1, 2, 3), 4), 1e-9)
    assertEquals(7, activeMax(Array(3, 4, 7, 1, 6, 8, 5, 2), Array(0, 1, 2, 3), 4), 1e-9)
    assertEquals(6, activeMax(Array(6, 4, 7, 1, 6, 8, 5, 2), Array(1, 3, 4, 6), 4), 1e-9)
    assertEquals(6, activeMax(Array(6, 4, 7, 1, 6, 8, 5, 2), Array(1, 3, 4, 6, 7, 8), 4), 1e-9)
  }

  @Test
  def test_argmax {
    //    assertEquals(log(0), activeLogSum(Array[Double](), Array(), 0), 1e-9)
    //    assertEquals(log(0), activeLogSum(Array[Double](arb, arb, arb), Array(), 0), 1e-9)
    assertEquals(0, argmax(Array(3), 1), 1e-9)
    assertEquals(0, argmax(Array(3, 5, 7), 1), 1e-9)
    assertEquals(1, argmax(Array(3, 4), 2), 1e-9)
    assertEquals(2, argmax(Array(3, 4, 7, 1), 4), 1e-9)
    assertEquals(2, argmax(Array(3, 4, 7, 1, 6, 8, 5, 2), 4), 1e-9)
    assertEquals(5, argmax(Array(3, 4, 7, 1, 6, 8, 5, 2), 8), 1e-9)
    assertEquals(2, argmax(Array(6, 4, 8, 1, 6, 8, 5, 2), 8), 1e-9)
  }

  @Test
  def test_activeArgmax {
    //    assertEquals(log(0), activeLogSum(Array[Double](), Array(), 0), 1e-9)
    //    assertEquals(log(0), activeLogSum(Array[Double](arb, arb, arb), Array(), 0), 1e-9)
    assertEquals(0, activeArgmax(Array(3), Array(0), 1), 1e-9)
    assertEquals(0, activeArgmax(Array(3), Array(0, 5, 6, 7), 1), 1e-9)
    assertEquals(1, activeArgmax(Array(3, 5, 7), Array(1, 5, 6, 7), 1), 1e-9)
    assertEquals(1, activeArgmax(Array(3, 4), Array(0, 1), 2), 1e-9)
    assertEquals(2, activeArgmax(Array(3, 4, 7, 1), Array(0, 1, 2, 3), 4), 1e-9)
    assertEquals(2, activeArgmax(Array(3, 4, 7, 1, 6, 8, 5, 2), Array(0, 1, 2, 3), 4), 1e-9)
    assertEquals(4, activeArgmax(Array(6, 4, 7, 1, 6, 8, 5, 2), Array(1, 3, 4, 6), 4), 1e-9)
    assertEquals(4, activeArgmax(Array(6, 4, 7, 1, 6, 8, 5, 2), Array(1, 3, 4, 6, 7, 8), 4), 1e-9)
    assertEquals(2, activeArgmax(Array(6, 4, 6, 1, 6, 8, 5, 2), Array(1, 2, 3, 4, 6), 5), 1e-9)
  }

  def arb = Random.nextDouble * 20

}
