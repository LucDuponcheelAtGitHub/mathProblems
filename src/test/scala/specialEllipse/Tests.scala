package specialEllipse

import org.scalatest.funsuite.AnyFunSuite

import scala.math.*

// equality up to ε because of Double precision
// therefore we define `~`
// some may succeed using `==`

val ε: Double = 0.00000000000001

extension (l: Double) def ~(r: Double): Boolean = abs(l - r) < ε

val allEqual: List[Double] => Boolean = ds => ds.forall(d => d ~ ds(0))

// P = (sqrt(2.0/3.0), -1.0/sqrt(2.0*3.0))
//
// P should be on the ellipse
//
// note that P is not the point shown on the ellipse of the picture,
// it is the point (not shown) on the ellipse which is
// symmetric w.r.t. the middle point of the ellipse (also not shown)

val P: Point = (x, y)

val x: Double = derivingSolutionForX.last

val y: Double = derivingSolutionForY.last

// should be 1.0
val v = x*x + 2.0*y*y

// should be π/3.0
val α: Double = derivingAngleBetweenFocusLinesAt(P).last

// should be π/4.0
val γ: Double = derivingAngleOfTangent.last

class Tests extends AnyFunSuite:

  test("verify deriving angle between focus lines at P") {
    allEqual(derivingAngleBetweenFocusLinesAt(P))
  }

  test("deriving equation for y at P") {
    derivingEquationForYAt(P)
  }

  test("verify deriving solution for y at P if α == π/3.0") {
    allEqual(derivingSolutionForY)
  }

  test("verify deriving solution for x at P if α == π/3.0") {
    allEqual(derivingSolutionForX)
  }

  test("verify deriving angle of tangent at P if α == π/3.0") {
    allEqual(derivingAngleOfTangent)
  }

  test("P is on ellipse") {
    v ~ (1.0)
  }

  test("angle between focus lines at P is π/3.0") {
    α ~ (π/3.0)
  }

  test("angle of tangent at P is π/4.0") {
    γ ~ (π/4.0)
  }
