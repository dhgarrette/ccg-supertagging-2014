package dhg.ccg.test

import org.apache.commons.math3.random.{ MersenneTwister, RandomGenerator }

object TestUtil {

  trait MockableRandomGenerator extends RandomGenerator {
    override def setSeed(seed: Int): Unit = ???
    override def setSeed(seed: Array[Int]): Unit = ???
    override def setSeed(seed: Long): Unit = ???
    override def nextBytes(bytes: Array[Byte]): Unit = ???
    override def nextInt(): Int = ???
    override def nextInt(n: Int): Int = ???
    override def nextLong(): Long = ???
    override def nextBoolean(): Boolean = ???
    override def nextFloat(): Float = ???
    override def nextDouble(): Double = ???
    override def nextGaussian(): Double = ???
  }

  case class DoubleIteratorRandomGenerator(it: Iterator[Double]) extends MockableRandomGenerator {
    override def nextDouble() = it.next()
  }

}
