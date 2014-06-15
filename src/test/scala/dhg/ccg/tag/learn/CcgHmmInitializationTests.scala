package dhg.ccg.tag.learn

import org.junit.Test
import dhg.util.StringUtil._
import org.junit.Assert._
import dhg.ccg.cat._
import dhg.ccg.prob._
import dhg.ccg.tag.TagDictionary
import dhg.ccg.tag.SimpleTagDictionary

// TODO: Test TagPriorTrInitializer in isolation
class CcgHmmInitializationTests {

  val A = AtomCat("<S>")
  val S = AtomCat("S")
  val N = AtomCat("N")
  val Z = AtomCat("<E>")

  @Test
  def test_CcgCombinabilityTrInitializer {
    val td = SimpleTagDictionary[String, Cat](Map(), "<S>", A, "<E>", Z, Set(),
      Set(N, N / N, N \ N, S \ N))

    val delegate = new TransitionInitializer[String, Cat] {
      override def apply(
        sentences: Vector[Vector[String]],
        initialTagdict: TagDictionary[String, Cat] //
        ): ConditionalProbabilityDistribution[Cat, Cat] = {
        new ConditionalProbabilityDistribution[Cat, Cat] {
          override def apply(x: Cat, given: Cat): Double = {
            given match {
              case N => x match {
                case A => 0.51
                case N => 0.11
                case N / N => 0.12
                case N \ N => 0.13
                case S \ N => 0.14
                case Z => 0.31
              }
              case N / N => x match {
                case A => 0.52
                case N => 0.15
                case N / N => 0.16
                case N \ N => 0.17
                case S \ N => 0.18
                case Z => 0.32
              }
              case N \ N => x match {
                case A => 0.53
                case N => 0.19
                case N / N => 0.21
                case N \ N => 0.22
                case S \ N => 0.23
                case Z => 0.33
              }
              case S \ N => x match {
                case A => 0.54
                case N => 0.19
                case N / N => 0.21
                case N \ N => 0.22
                case S \ N => 0.23
                case Z => 0.34
              }
              case A => x match {
                case A => 0.55
                case N => 0.19
                case N / N => 0.21
                case N \ N => 0.22
                case S \ N => 0.23
                case Z => 0.35
              }
              case Z => x match {
                case A => 0.56
                case N => 0.41
                case N / N => 0.42
                case N \ N => 0.43
                case S \ N => 0.44
                case Z => 0.45
              }
            }
          }
          override def sample(given: Cat): Cat = ???
        }
      }
    }

    val catPriorTrInit = new TagPriorTrInitializer(new TagPriorInitializer[String, Cat] {
      def apply(sentences: Vector[Vector[String]], initialTagdict: TagDictionary[String, Cat]): ProbabilityDistribution[Cat] =
        new SimpleProbabilityDistribution[Cat](Map(
          (A) -> 0.61,
          (N) -> 0.62,
          (N / N) -> 0.63,
          (N \ N) -> 0.64,
          (S \ N) -> 0.65,
          (Z) -> 0.66))
    })

    //    val init = new TagPriorTrInitializer[String, Cat](
    //    		new CatComplexityInitializer())

    //    val init =
    //      new CcgCombinabilityTrInitializer[String](delegate, canCombine, combinableTransitionMass = 0.8)

    val combinableTransitionMass = 0.8
    val catpriorMass = 0.6
    val canCombine = new SimpleCatCanCombine(Vector(FA, BA), A, Z)
    val combineTrInit = new CcgCombinabilityTrInitializer[String](delegate, canCombine, combinableTransitionMass)
    val trInit = new InterpolatingTransitionInitializer[String, Cat](Vector(combineTrInit -> (1 - catpriorMass), catPriorTrInit -> catpriorMass))

    val tr1 = catPriorTrInit(Vector(), td)
    val tr2 = trInit(Vector(), td)

    println(f"${0.12 * 0.6} =?= ${tr1(N \ N, N)}")
    println(f"${0.12 * 0.6} =?= ${tr1(N / N, N)}")
    println(f"${0.12 * 0.6 * 0.8} =?= ${tr2(N \ N, N)}")
    println(f"${0.12 * 0.6 * (1 - 0.8)} =?= ${tr2(N / N, N)}")
  }

  @Test
  def test_TagPriorTrInitializer {
    ???
  }

  object BadCat extends Cat {
    def size: Int = ???
    def complexity: Int = ???
    def u(b: Cat): Boolean = ???
  }

}
