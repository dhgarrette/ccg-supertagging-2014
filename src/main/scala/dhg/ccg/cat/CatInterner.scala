package dhg.ccg.cat

import dhg.util.CollectionUtil._
import dhg.util.StringUtil._
import scalaz.Scalaz._
import scala.collection.mutable

/**
 * Parse category strings to create Cat objects, but create a pool of
 * Cat objects that can be reused (including as sub-expressions) to
 * save lots of memory.
 */
final class CatInterner(removeFeatures: Boolean = false) extends (String => Cat) {

  val CcgBankAtomRe = """([^\[\]]+)(\[([^\[\]]+)\])?""".r
  val TutBankAtomRe = """([^\[\]:]+)(:([^\[\]:]+))?""".r

  private[this] val cache = mutable.Map[String, Cat]()

  override final def apply(s: String): Cat = {
    try {
      val s2 = if (removeFeatures) s.replaceAll("""\[[^\[\]]+\]""", "") else s
      cache.getOrElseUpdate(s2,
        tokensToCat(s2.replaceAll("\\s+", "").replaceAll("([\\\\/()])", " $1 ").lsplit("\\s+").filter(_.nonEmpty)))
    }
    catch {
      case e: RuntimeException =>
        println(f"BAD TAG: $s")
        throw new RuntimeException(f"failed to parse cat: `$s`", e)
    }
  }

  final def apply(strings: TraversableOnce[String]): CatInterner = {
    strings.foreach(this)
    this
  }

  private[this] final def tokensToCat(v: Vector[String]): Cat = {
    v match {
      case Vector(x) =>
        val (atomcat, atom, feature) =
          x match {
            case atomcat @ CcgBankAtomRe(atom, _, feature) => (atomcat, atom, feature)
            case atomcat @ TutBankAtomRe(atom, _, feature) => (atomcat, atom, feature)
            case _ => sys.error(f"Bad atom: $x")
          }
        if (removeFeatures)
          cache.getOrElseUpdate(atom, AtomCat(atom))
        else
          cache.getOrElseUpdate(atomcat, AtomCat(atom, Option(feature)))

      case _ if v.size >= 5 =>
        cache.getOrElseUpdate(v.mkString, {
          val opIdx = getMainOpIdx(v)
          val left = tokensToCat(v.slice(1, opIdx))
          val right = tokensToCat(v.slyce(opIdx + 1, -1))
          v(opIdx) match {
            case "/" => FCat(left, right)
            case "\\" => BCat(left, right)
          }
        })
    }
  }

  private[this] final def getMainOpIdx(v: Vector[String]) = {
    v.zipWithIndex.tail.foldLeftWhile((0, none[Int]))((z, _) => z._2.isEmpty) {
      case ((pc, None), (tok, i)) =>
        tok match {
          case "(" => (pc + 1, none)
          case ")" => (pc - 1, none)
          case "/" | "\\" if pc == 0 => (0, Some(i))
          case _ => (pc, none)
        }
    } match {
      case (_, Some(opIdx)) => opIdx
      case _ => sys.error(s"failed to find main op in ${v.mkString}")
    }
  }

  def unapply(s: String): Option[Cat] = Some(this(s))

}

object CatInterner {
  def main(args: Array[String]): Unit = {
    val p = new CatInterner
    println(p("""((S\NP)\((S\NP)/NP)[conj])"""))

  }
}