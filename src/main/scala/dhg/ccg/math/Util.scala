package dhg.ccg.math

import dhg.util.CollectionUtil._
import dhg.util.Time._
import dhg.util.FileUtil._
import dhg.util.StringUtil._
import scala.annotation.tailrec
import scala.math.{ pow, exp, log }
import org.apache.commons.math3.random.{ RandomGenerator }

object Util {

  /**
   * Sums together things in log space.
   * @return log(exp(a) + exp(b))
   *
   * stolen from breeze
   */
  def logSum(a: Double, b: Double) = {
    if (a.isNegInfinity) b
    else if (b.isNegInfinity) a
    else if (a < b) b + scala.math.log1p(exp(a - b))
    else a + scala.math.log1p(exp(b - a))
  }

  /**
   * Sums together things in log space.
   * @return log(\sum exp(a_i))
   *
   * stolen from breeze
   */
  def logSum(a: Double, b: Double, c: Double*): Double = {
    if (c.length == 0)
      logSum(a, b)
    else
      logSum(logSum(a, b) +: c)
  }

  /**
   * Sums together things in log space.
   * @return log(\sum exp(a_i))
   *
   * stolen from breeze
   */
  def logSum(iter: Iterator[Double], max: Double): Double = {
    require(iter.hasNext)
    if (max.isInfinite) {
      max
    }
    else {
      val aux = (0.0 /: iter) {
        (acc, x) => if (x.isNegInfinity) acc else acc + exp(x - max)
      }
      if (aux != 0)
        max + scala.math.log(aux)
      else
        max
    }
  }

  /**
   * Sums together things in log space.
   * @return log(\sum exp(a_i))
   *
   * stolen from breeze
   */
  def logSum(a: Seq[Double]): Double = {
    a.length match {
      case 0 => Double.NegativeInfinity
      case 1 => a(0)
      case 2 => logSum(a(0), a(1))
      case _ => logSum(a.iterator, a reduceLeft (_ max _))
    }
  }

  /**
   * Sums together the first length elements in log space.
   * The length parameter is used to make things faster.
   *
   * This method needs to be fast. Don't scala-ify it.
   * @the  log(\sum^length exp(a_i))
   *
   * stolen from breeze
   */
  def logSum(a: Array[Double], length: Int): Double = {
    length match {
      case 0 => Double.NegativeInfinity
      case 1 => a(0)
      case 2 => logSum(a(0), a(1))
      case _ =>
        val m = max(a, length)
        if (m.isInfinite) m
        else {
          var i = 0
          var accum = 0.0
          while (i < length) {
            accum += scala.math.exp(a(i) - m)
            i += 1
          }
          m + scala.math.log(accum)
        }
    }
  }

  /**
   * fast versions of max. Useful for the fast logsum.
   *
   * stolen from breeze
   */
  def max(a: Array[Double], length: Int) = {
    if (length == 0 || length > a.length) {
      throw new IllegalArgumentException(s"Passed in a length of $length to max, for an array of type ${a.length}")
    }
    var i = 1
    var max = a(0)
    while (i < length) {
      if (a(i) > max) max = a(i)
      i += 1
    }
    max
  }

  /**
   * sample from the distribution
   */
  def choose(dist: Array[Double], count: Int, rand: RandomGenerator): Int = {
    if (count == 0 || count > dist.length) throw new IllegalArgumentException(s"Passed in a count of $count to choose, for an array of type ${dist.length}")
    if (count == 1) return 0
    val r = rand.nextDouble
    var s = sum(dist, count) * r
    var i = 0
    while (i < count) {
      s -= dist(i)
      if (s < 0) return i
      i += 1
    }
    sys.error(f"No value chosen!  ${dist.mkString("[", ", ", "]")}, r=$r%.2f")
  }

  /**
   * Don't let all active values to be -inf
   */
  def logChoose(logDist: Array[Double], active: Array[Int], activeCount: Int, rand: RandomGenerator): Int = {
    if (activeCount == 0) throw new IllegalArgumentException("cannot `choose` from an empty distribution")
    if (activeCount == 1) return active(0)
    val logSum = activeLogSum(logDist, active, activeCount)
    var prob = rand.nextDouble
    var i = 0
    while (i < activeCount) {
      val ai = active(i)
      prob -= exp(logDist(ai) - logSum)
      if (prob < 0) return ai
      i += 1
    }
    sys.error(f"No value chosen!  ${logDist.mkString("[", ", ", "]")}, ${active.take(activeCount).mkString("[[", ", ", "]]")}${active.drop(activeCount).mkString("", " ", "]")}")
  }

  def normalize(a: Array[Double], count: Int): Unit = {
    var s = sum(a, count)
    var i = 0
    while (i < count) {
      a(i) /= s
      i += 1
    }
  }

  def activeNormalize(a: Array[Double], active: Array[Int], activeCount: Int): Unit = {
    var s = activeSum(a, active, activeCount)
    var i = 0
    while (i < activeCount) {
      val ai = active(i)
      a(ai) /= s
      i += 1
    }
  }

  def normalizeAndLog(a: Array[Double], count: Int): Unit = {
    var s = sum(a, count)
    var i = 0
    while (i < count) {
      a(i) = log(a(i) / s)
      i += 1
    }
  }

  def activeNormalizeAndLog(a: Array[Double], active: Array[Int], activeCount: Int): Unit = {
    var s = activeSum(a, active, activeCount)
    var i = 0
    while (i < activeCount) {
      val ai = active(i)
      a(ai) = log(a(ai) / s)
      i += 1
    }
  }

  def logNormalize(logData: Array[Double], count: Int): Unit = {
    var logsum = logSum(logData, count)
    var i = 0
    while (i < count) {
      logData(i) = logData(i) - logsum
      i += 1
    }
  }

  def activeLogNormalize(logData: Array[Double], active: Array[Int], activeCount: Int): Unit = {
    var logSum = activeLogSum(logData, active, activeCount)
    var i = 0
    while (i < activeCount) {
      val ai = active(i)
      logData(ai) = logData(ai) - logSum
      i += 1
    }
  }

  /**
   * makes log-normalized probabilities
   */
  def convertToLogDirichletDraw(counts: Array[Double], count: Int, rand: RandomGenerator): Unit = {
    var i = 0
    while (i < count) {
      val gd = gammaLogDraw(counts(i), rand)
      counts(i) = gd
      i += 1
    }
    logNormalize(counts, count)
  }

  /**
   * makes log-normalized probabilities
   */
  def convertActiveToLogDirichletDraw(counts: Array[Double], active: Array[Int], activeCount: Int, rand: RandomGenerator): Unit = {
    var i = 0
    while (i < activeCount) {
      val ai = active(i)
      val gd = gammaLogDraw(counts(ai), rand)
      counts(ai) = gd
      i += 1
    }
    activeLogNormalize(counts, active, activeCount)
  }

  def gammaLogDraw(shape: Double, rand: RandomGenerator): Double = {
    if (shape < 1) {
      // adapted from numpy distributions.c which is Copyright 2005 Robert Kern (robert.kern@gmail.com) under BSD
      @tailrec
      def rec: Double = {
        val u = rand.nextDouble
        val v = -math.log(rand.nextDouble)
        val logU = log(u)
        if (logU <= math.log1p(-shape)) {
          val logV = log(v)
          val logX = logU / shape
          if (logX <= logV) logX
          else rec
        }
        else {
          val y = -log((1 - u) / shape)
          val logX = math.log(1.0 - shape + shape * y) / shape
          if (logX <= math.log(v + y)) logX
          else rec
        }
      }
      rec
    }
    else math.log(gammaDraw(shape, rand))
  }

  def gammaDraw(shape: Double, rand: RandomGenerator): Double = {
    if (shape == 1.0) {
      -math.log(rand.nextDouble)
    }
    else if (shape < 1.0) {
      // from numpy distributions.c which is Copyright 2005 Robert Kern (robert.kern@gmail.com) under BSD
      @tailrec
      def rec: Double = {
        val u = rand.nextDouble
        val v = -math.log(rand.nextDouble)
        if (u <= 1.0 - shape) {
          val x = pow(u, 1.0 / shape)
          if (x <= v) x
          else rec
        }
        else {
          val y = -log((1 - u) / shape)
          val x = pow(1.0 - shape + shape * y, 1.0 / shape)
          if (x <= (v + y)) x
          else rec
        }
      }
      rec
    }
    else {
      // from numpy distributions.c which is Copyright 2005 Robert Kern (robert.kern@gmail.com) under BSD
      val d = shape - 1.0 / 3.0
      val c = 1.0 / math.sqrt(9.0 * d)
      var r = 0.0
      var ok = false
      while (!ok) {
        var v = 0.0
        var x = 0.0
        do {
          x = rand.nextGaussian
          v = 1.0 + c * x
        } while (v <= 0)

        v = v * v * v
        val x2 = x * x
        val u = rand.nextDouble
        if (u < 1.0 - 0.0331 * (x2 * x2)
          || log(u) < 0.5 * x2 + d * (1.0 - v + log(v))) {
          r = d * v
          ok = true
        }
      }
      r
    }
  }

  def sum(a: Array[Double], count: Int): Double = {
    var accum = 0.0
    var i = 0
    while (i < count) {
      accum += a(i)
      i += 1
    }
    accum
  }

  def activeSum(a: Array[Double], active: Array[Int], activeCount: Int): Double = {
    var accum = 0.0
    var i = 0
    while (i < activeCount) {
      accum += a(active(i))
      i += 1
    }
    accum
  }

  def activeLogSum(a: Array[Double], active: Array[Int], activeCount: Int): Double = {
    if (activeCount == 0) Double.NegativeInfinity
    else if (activeCount == 1) a(active(0))
    else if (activeCount == 2) logSum(a(active(0)), a(active(1)))
    else {
      val m = activeMax(a, active, activeCount)
      //        if (m.isNegInfinity) m
      //        else {
      var i = 0
      var accum = 0.0
      while (i < activeCount) {
        accum += scala.math.exp(a(active(i)) - m)
        i += 1
      }
      m + scala.math.log(accum)
    }
  }

  /**
   *
   */
  def activeMax(a: Array[Double], active: Array[Int], activeCount: Int) = {
    var max = a(active(0))
    var i = 1
    while (i < activeCount) {
      if (a(active(i)) > max)
        max = a(active(i))
      i += 1
    }
    max
  }

  /**
   *
   */
  def argmax(a: Array[Double], length: Int) = {
    //    if (length == 0 || length > a.length) {
    //      throw new IllegalArgumentException(s"Passed in a length of $length to choose, for an array of type ${a.length}")
    //    }
    var max = a(0)
    var maxIdx = 0
    var i = 1
    while (i < length) {
      if (a(i) > max) {
        max = a(i)
        maxIdx = i
      }
      i += 1
    }
    maxIdx
  }

  /**
   *
   */
  def activeArgmax(a: Array[Double], active: Array[Int], activeCount: Int) = {
    //    if (activeCount == 0) {
    //      throw new IllegalArgumentException("cannot `activeArgmax` from an empty array")
    //    }
    var maxIdx = active(0)
    var max = a(maxIdx)
    var i = 1
    while (i < activeCount) {
      if (a(active(i)) > max) {
        maxIdx = active(i)
        max = a(maxIdx)
      }
      i += 1
    }
    maxIdx
  }

}
