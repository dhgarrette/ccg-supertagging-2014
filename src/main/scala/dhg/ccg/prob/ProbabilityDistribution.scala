package dhg.ccg.prob

import scala.util.Random
import dhg.util.CollectionUtil._
import com.typesafe.scalalogging.slf4j.{ StrictLogging => Logging }

abstract class ProbabilityDistribution[B]() {
  def apply(b: B): Double
  def sample(): B
  def defaultProb: Double
}

abstract class AbstractProbabilityDistribution[B](totalAddition: Double = 0.0) extends ProbabilityDistribution[B] {
  protected[this] def allKnownBs: Set[B]
  protected[this] def counts(b: B): Double
  protected[this] def defaultCount: Double

  final private[this] def allKnownProbs = allKnownBs.iterator.mapTo(apply)
  final def defaultProb = defaultCount / smoothedCountSum

  final private[this] lazy val smoothedCountSum = allKnownBs.sumBy(counts) + totalAddition

  final def apply(b: B): Double = {
    if (smoothedCountSum == 0.0)
      0.0
    else
      counts(b) / smoothedCountSum
  }

  final def sample(): B = {
    assert(allKnownBs.nonEmpty)
    val orderedBs = allKnownBs.toVector.mapTo(counts).sortBy(-_._2)
    var accum = Random.nextDouble * smoothedCountSum
    val it = orderedBs.iterator
    while (it.hasNext) {
      val (item, count) = it.next
      accum -= count
      if (accum <= 0)
        return item
    }
    sys.error(f"nothing sampled!  orderedBs=${orderedBs.map(_._2)}")
  }

  final override def toString = f"PD(${allKnownBs.toVector.mapTo(counts).sortBy(-_._2).map(_._1).mapTo(apply).map { case (k, v) => f"$k -> $v" }.mkString(", ")})"
}

object ProbabilityDistribution {
  def empty[B] = new SimpleProbabilityDistribution[B](Map())
}

class SimpleProbabilityDistribution[B](unsmoothedCounts: Map[B, Double])
  extends LaplaceProbabilityDistribution[B](unsmoothedCounts, None, None, 0.0)

class LaplaceProbabilityDistribution[B](
  unsmoothedCounts: Map[B, Double],
  knownBs: Option[Set[B]],
  excludedBs: Option[B => Boolean],
  lambda: Double,
  totalAddition: Double = 0.0)
  extends AbstractProbabilityDistribution[B](totalAddition = totalAddition)
  with Logging {

  val allKnownBs = (knownBs.getOrElse(Set()) | unsmoothedCounts.keySet).filterNot(excludedBs.getOrElse(Set()))
  def counts(b: B) = if (excludedBs.isDefined && excludedBs.get(b)) 0.0 else (unsmoothedCounts.getOrElse(b, 0.0) + lambda)
  def defaultCount: Double = lambda
}

class DefaultedProbabilityDistribution[B](
  smoothedCounts: Map[B, Double],
  knownBs: Option[Set[B]],
  excludedBs: Option[B => Boolean],
  val defaultCount: Double,
  totalAddition: Double = 0.0)
  extends AbstractProbabilityDistribution[B](totalAddition = totalAddition)
  with Logging {

  def this(smoothedCounts: Map[B, Double]) = this(smoothedCounts, None, None, 0.0)

  val allKnownBs = (knownBs.getOrElse(Set()) | smoothedCounts.keySet).filterNot(excludedBs.getOrElse(Set()))
  def counts(b: B) = if (excludedBs.isDefined && excludedBs.get(b)) 0.0 else smoothedCounts.getOrElse(b, defaultCount)
}

//
//
//

trait ConditionalProbabilityDistribution[A, B] {
  def apply(x: B, given: A): Double
  def sample(given: A): B
}

class SimpleConditionalProbabilityDistribution[A, B](
  conditionedDistributions: Map[A, ProbabilityDistribution[B]],
  knownAs: Option[Set[A]],
  excludedAs: Option[A => Boolean],
  default: ProbabilityDistribution[B])
  extends ConditionalProbabilityDistribution[A, B] {

  def this(conditionedDistributions: Map[A, ProbabilityDistribution[B]]) = this(conditionedDistributions, None, None, ProbabilityDistribution.empty[B])

  private[this] val allKnownAs = (knownAs.getOrElse(Set()) | conditionedDistributions.keySet).filterNot(excludedAs.getOrElse(Set()))
  private[this] def allKnownConditionedDistributions = allKnownAs.iterator.mapTo(given => conditionedDistributions.getOrElse(given, default))

  def apply(x: B, given: A): Double = {
    if (excludedAs.isDefined && excludedAs.get(given))
      0.0
    else
      conditionedDistributions.getOrElse(given, default)(x)
  }

  def sample(given: A): B = {
    if (excludedAs.isDefined && excludedAs.get(given))
      sys.error(f"cannot sample from $given")
    else
      conditionedDistributions.getOrElse(given, default).sample
  }
}

class InterpolatingConditionalProbabilityDistribution[Word, Tag](
  delegates: Vector[(ConditionalProbabilityDistribution[Tag, Tag], Double)])
  extends ConditionalProbabilityDistribution[Tag, Tag] {

  def apply(x: Tag, given: Tag): Double = {
    delegates.sumBy { case (d, w) => d(x, given) * w }
  }

  def sample(given: Tag): Tag = ???
}

class ReversingConditionalProbabilityDistribution[A, B](delegate: ConditionalProbabilityDistribution[B, A]) extends ConditionalProbabilityDistribution[A, B] {
  def apply(x: B, given: A): Double = delegate(given, x)
  def sample(given: A): B = ???
}
