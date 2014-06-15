package dhg.ccg.cat

import scala.annotation.tailrec

trait Cat {
  def size: Int
  def complexity: Int
  def /(b: Cat) = FCat(this, b)
  def \(b: Cat) = BCat(this, b)
  def u(b: Cat): Boolean
}

object / { def unapply(c: Cat) = c match { case FCat(a, b) => Some((a, b)); case _ => None } }
object \ { def unapply(c: Cat) = c match { case BCat(a, b) => Some((a, b)); case _ => None } }
object \/ { def unapply(c: Cat) = c match { case a / b => Some((a, b)); case a \ b => Some((a, b)); case _ => None } }

case class AtomCat(atom: String, feature: Option[String] = None) extends Cat {
  override def size = 1
  override def complexity = 1
  override def u(b: Cat) = b match {
    case AtomCat(`atom`, bFeature) => feature == bFeature || feature.isEmpty || bFeature.isEmpty
    case _ => false
  }
  def apply(f: String) = { assert(feature.isEmpty); AtomCat(atom, Some(f)) }
  def base = AtomCat(atom)
  override def toString = feature.fold(atom)(f => f"$atom[$f]")
}

abstract class OpCat(left: Cat, op: String, right: Cat) extends Cat {
  final override def size = left.size + right.size
  final override def complexity = 1 + left.complexity + right.complexity
  override def toString = s"($left$op$right)"
}

case class FCat(left: Cat, right: Cat) extends OpCat(left, "/", right) {
  override def u(b: Cat) = b match {
    case FCat(bl, br) => (left u bl) && (right u br)
    case _ => false
  }
}

case class BCat(left: Cat, right: Cat) extends OpCat(left, "\\", right) {
  override def u(b: Cat) = b match {
    case BCat(bl, br) => (left u bl) && (right u br)
    case _ => false
  }
}

case class ConjCat(c: Cat) extends Cat {
  def size: Int = 10000 + c.size
  def complexity: Int = 1000
  override def /(b: Cat) = FCat(AtomCat("???"), b)
  override def \(b: Cat) = BCat(AtomCat("???"), b)
  def u(b: Cat): Boolean = b match { case ConjCat(bInner) if c u bInner => true; case _ => false }
  override def toString = s"$c[conj]"
}

object Cat {
  implicit object CatOrdering extends Ordering[Cat] {
    def compare(a: Cat, b: Cat): Int = (a, b) match {
      case _ if (a.size < b.size) => -1
      case _ if (a.size > b.size) => 1
      case (AtomCat(x, xf), AtomCat(y, yf)) => Ordering[(String, Option[String])].compare((x, xf), (y, yf))
      case (FCat(_, _), BCat(_, _)) => -1
      case (BCat(_, _), FCat(_, _)) => 1
      case (w \/ x, y \/ z) if w == x => CatOrdering.compare(y, z)
      case (w \/ x, y \/ z) => CatOrdering.compare(w, x)
    }
  }
}

//
//
//

trait CatCanCombine {
  def apply(a: Cat, b: Cat): Boolean

  def rules: Vector[TrueCcgRule]
  def startCat: Cat
  def endCat: Cat
}

final class SimpleCatCanCombine(val rules: Vector[TrueCcgRule], val startCat: Cat, val endCat: Cat) extends CatCanCombine {

  override def apply(a: Cat, b: Cat): Boolean = {
    if (a == startCat)
      return !removeAllFollowing(b).isInstanceOf[BCat]
    if (b == endCat)
      return !removeAllPreceding(a).isInstanceOf[FCat]
    for {
      ax <- removePreceding(a)
      bx <- removeFollowing(b)
      rule <- rules
      if rule(ax, bx).isDefined
    } return true
    return false
  }

  private[this] def removePreceding(a: Cat): List[Cat] = {
    @tailrec def f(a: Cat, accum: List[Cat]): List[Cat] =
      a match {
        case l \ r => f(l, l :: accum)
        case _ => accum
      }
    f(a, List(a))
  }

  private[this] def removeFollowing(b: Cat): List[Cat] = {
    @tailrec def f(b: Cat, accum: List[Cat]): List[Cat] =
      b match {
        case l / r => f(l, l :: accum)
        case _ => accum
      }
    f(b, List(b))
  }

  @tailrec
  private[this] def removeAllPreceding(a: Cat): Cat = {
    a match {
      case l \ r => removeAllPreceding(l)
      case _ => a
    }
  }

  @tailrec
  private[this] def removeAllFollowing(b: Cat): Cat = {
    b match {
      case l / r => removeAllFollowing(l)
      case _ => b
    }
  }

  override def toString = f"CatCanCombine(${rules.mkString(", ")})"
}
