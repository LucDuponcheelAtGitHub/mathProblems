package specialEllipse

import scala.math.*

// the general equation of an ellipse
// with major axis a and minor axis b
// is x*x/a*a + y*y/b*b = 1
//
// for the special ellipse we use a = 1 and b = 1/sqrt(2) (cfr A4 paper)
// and therefore the equation of the special ellipse is x*x + 2*y*y = 1

val π: Double = Pi

type Point = Tuple2[Double, Double]

val derivingTangensOfAngleBetweenFocusLinesAt: Point => List[Double] =

  val slopeOfLeftFocusLinesAt: Point => Double = (x, y) => y / (x + 1.0 / sqrt(2.0))

  val slopeOfRightFocusLinesAt: Point => Double = (x, y) => y / (x - 1.0 / sqrt(2.0))

  val angleBetweenFocusLinesAt: Point => Double = (x, y) =>
    atan(slopeOfLeftFocusLinesAt(x, y)) - atan(slopeOfRightFocusLinesAt(x, y))

  (x, y) =>
    List(
      tan(angleBetweenFocusLinesAt(x, y)),
      tan(atan(slopeOfLeftFocusLinesAt(x, y)) - atan(slopeOfRightFocusLinesAt(x, y))),
      tan(atan(y / (x + 1.0 / sqrt(2.0))) - atan(y / (x - 1.0 / sqrt(2.0)))),
      // tan(α - β) = (tan(α) - tan(β))/(1.0 + tan(α) * tan(β))
      (tan(atan(y / (x + 1.0 / sqrt(2.0)))) - tan(
        atan(y / (x - 1.0 / sqrt(2.0)))
      )) / (1.0 + tan(atan(y / (x - 1.0 / sqrt(2.0)))) * tan(
        atan(y / (x + 1.0 / sqrt(2.0)))
      )),
      ((y / (x + 1.0 / sqrt(2.0))) - (y / (x - 1.0 / sqrt(
        2.0
      )))) / (1.0 + (y / (x - 1.0 / sqrt(2.0))) * (y / (x + 1.0 / sqrt(2.0)))),
      (((-y * x - y / sqrt(2.0) + y * x - y / sqrt(
        2.0
      ))) / (x * x - 1.0 / 2.0)) / (1.0 + (y * y / (x * x - 1.0 / 2.0))),
      (((-y * x - y / sqrt(2.0) + y * x - y / sqrt(
        2.0
      ))) / (x * x - 1.0 / 2.0)) / ((x * x - 1.0 / 2.0 + y * y) / (x * x - 1.0 / 2.0)),
      ((-y * x - y / sqrt(2.0) + y * x - y / sqrt(2.0))) / (x * x - 1.0 / 2.0 + y * y),
      (-y / sqrt(2.0) - y / sqrt(2.0)) / (x * x - 1.0 / 2.0 + y * y),
      (-y / sqrt(2.0) - y / sqrt(2.0)) / (1.0 - 2.0 * y * y - 1.0 / 2.0 + y * y),
      (-y / sqrt(2.0) - y / sqrt(2.0)) / (1.0 / 2.0 - y * y),
      (-2.0 * y / sqrt(2.0)) / (1.0 / 2.0 - y * y)
    )

// if angleBetweenFocusLinesAt(x, y) = π/3.0
// then tan(angleBetweenFocusLinesAt(x, y)) = tan(π/3.0) = sqrt(3.0)

val derivingEquationForYAt: Point => List[Double] =
  (x, y) =>
    List(
      sqrt(2.0) * (sqrt(3.0) - ((-2.0 * y / sqrt(2.0)) / (1.0 / 2.0 - y * y))),
      sqrt(2.0) * (sqrt(3.0) * (1.0 / 2.0 - y * y) - (-2.0 * y / sqrt(2.0))),
      sqrt(2.0) * (sqrt(3.0) * (1.0 / 2.0 - y * y) + (2.0 * y / sqrt(2.0))),
      -sqrt(2.0) * sqrt(3.0) * y * y + 2.0 * y + sqrt(3.0) / sqrt(2.0),
      -sqrt(2.0 * 3.0) * y * y + 2.0 * y + sqrt(2.0 * 3.0) / 2.0
    )

// solution of a*y*y + 2*b*y + c = 0 is
// y = -b + sqrt(b*b - a*c))/a

val derivingSolutionForY: List[Double] = List(
  (-1.0 + sqrt(1.0 + 3.0)) / -sqrt(2.0 * 3.0),
  (1.0 - sqrt(1.0 + 3.0)) / sqrt(2.0 * 3.0),
  (1.0 - sqrt(4.0)) / sqrt(2.0 * 3.0),
  (1.0 - 2.0) / sqrt(2.0 * 3.0),
  -1.0 / sqrt(2.0 * 3.0)
)

// x = sqrt(1.0 - 2.0*y*y)

val derivingSolutionForX: List[Double] = List(
  sqrt(1.0 - 2.0 * (-1.0 / sqrt(2.0 * 3.0)) * (-1.0 / sqrt(2.0 * 3.0))),
  sqrt(1.0 - 2.0 * (1.0 / sqrt(2.0 * 3.0)) * (1.0 / sqrt(2.0 * 3.0))),
  sqrt(1.0 - 2.0 / (2.0 * 3.0)),
  sqrt(1.0 - 1.0 / 3.0),
  sqrt(2.0 / 3.0)
)

// ∃γ P = (cos(γ), sin(γ)/sqrt(2.0))
// the slope of the tangent at P is -sin(γ) / (cos(γ)/sqrt(2.0))
// in other words, without using γ,
// the slope of the tangent at P is -(y*sqrt(2.0)) / (x/sqrt(2.0))

val derivingAngleOfTangent: List[Double] = List(
  atan((1.0 / sqrt(2.0 * 3.0) * sqrt(2.0)) / ((sqrt(2.0 / 3.0)) / sqrt(2.0))),
  atan((1.0 / (sqrt(3.0))) / (1.0 / sqrt(3.0))),
  atan(1.0)
)

@main def main() =

  val ε: Double = 0.00000000000001

  extension (l: Double) def ~(r: Double): Boolean = abs(l - r) < ε

  val allEqual: List[Double] => Boolean = ds => ds.forall(d => d ~ ds(0))

  // P = (sqrt(2.0/3.0), -1.0/sqrt(2.0*3.0))
  // P should be on the ellipse
  //
  // note that P is not the point shown on the ellipse of the image,
  // it is the point (not shown) on the ellipse which is
  // symmetric w.r.t. the center of the ellipse (also not shown)

  val x: Double = derivingSolutionForX.last

  val y: Double = derivingSolutionForY.last

  val P: Point = (x, y)

  import scala.io.AnsiColor.*

  val test: String => Boolean => Unit =
    message =>
      booleanResult =>
        print(s"$message")
        if (booleanResult) {
          println(s" is ${GREEN}OK${RESET}")
        } else {
          println(s" is ${RED}KO${RESET}")
        }

  test("deriving angle between focus lines at P") {
    allEqual(derivingTangensOfAngleBetweenFocusLinesAt(P))
  }

  test("deriving equation for y at P") {
    allEqual(derivingEquationForYAt(P))
  }

  test("deriving solution for y at P if α == π/3.0") {
    allEqual(derivingSolutionForY)
  }

  test("deriving solution for x at P if α == π/3.0") {
    allEqual(derivingSolutionForX)
  }

  test("deriving angle of tangent at P if α == π/3.0") {
    allEqual(derivingAngleOfTangent)
  }

  val v = x * x + 2.0 * y * y

  test("P is on ellipse") {
    v ~ 1.0
  }

  val α: Double = atan(derivingTangensOfAngleBetweenFocusLinesAt(P).last)

  test("angle between focus lines at P is π/3.0") {
    α ~ (π / 3.0)
  }

  val γ: Double = derivingAngleOfTangent.last

  test("angle of tangent at P is π/4.0") {
    γ ~ (π / 4.0)
  }
