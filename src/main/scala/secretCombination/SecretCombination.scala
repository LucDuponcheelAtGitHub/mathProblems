package secretCombination

import scala.math

def unique[Z]: List[Z] => Boolean =
  case List(z) => true
  case _       => false

def notUnique[Z]: List[Z] => Boolean = zs => !(unique(zs))

type Choices = List[Int]

def strictlyAscending: Choices => Boolean =
  case Nil      => true
  case _ :: Nil => true
  case z :: zs  => strictlyAscending(zs) && z < zs.head

val choices: Int => Int => List[Choices] =
  n =>
    case 0 => List(List())
    case m =>
      (for {
        z <- 1 to n
        zs <- choices(n)(m - 1)
        if (strictlyAscending(z :: zs))
      } yield {
        z :: zs
      }).toList

type Sum = Int

type Product = Int

val choicesWithSumsAndProducts: Tuple2[Int, Int] => List[(Choices, Sum, Product)] =
  (n, m) =>
    for {
      choice <- choices(n)(m)
    } yield {
      (choice, choice.sum, choice.product)
    }

val productsForSum: Int => List[(Choices, Sum, Product)] => Choices =
  s1 =>
    csps =>
      for {
        (cs, s2, p) <- csps
        if s1 == s2
      } yield {
        p
      }

val sumsForProduct: Int => List[(Choices, Sum, Product)] => Choices =
  p1 =>
    csps =>
      for {
        (cs, s, p2) <- csps
        if p1 == p2
      } yield {
        s
      }

val subtractions: Choices => Choices =
  cs =>
    for {
      c <- cs.init
    } yield {
      cs(cs.indexOf(c) + 1) - c
    }

// you can choose between forall and exists
val subtractionsDiffer: Choices => Choices => Boolean =
  // cs1 => cs2 => subtractions(cs1).zip(subtractions(cs2)).forall(_ != _)
  cs1 => cs2 => subtractions(cs1).zip(subtractions(cs2)).exists(_ != _)

val uniqueSubtractions: List[Choices] => Choices => Boolean =
  css2 =>
    cs1 =>
      (for {
        cs2 <- css2
        if (cs1 != cs2)
      } yield {
        cs2
      }).forall(subtractionsDiffer(cs1))

// alice betty and colin are sitting together

// alice tells betty and colin that the combination,
// consisting of m strictly ascending natural numbers between 1 to n,
// is one you cannot find from any sum
val alice: List[(Choices, Sum, Product)] => List[(Choices, Sum, Product)] =
  csps =>
    val aliceFilter: Sum => Boolean =
      s => 
        (for {
          p <- productsForSum(s)(csps)
        } yield {
          sumsForProduct(p)(csps)
        }).exists(notUnique)
    for {
      (c, s, p) <- csps
      if (aliceFilter(s))
    } yield {
      (c, s, p)
    }

// alice gives the product to betty and tells betty and colin that
// betty should know how to find the combination from that product
// (colin does not know the product (yet))
val betty: List[(Choices, Sum, Product)] => List[Choices] =
  csps =>
    val bettyFilter: Product => Boolean = 
      p => unique(sumsForProduct(p)(csps))
    for {
      (c, s, p) <- csps
      if (bettyFilter(p))
    } yield {
      c
    }

// alice tells (betty and) colin that,
// from all combination choices there is
// one combination with unique subtractions
// more precisely, for all other combination choices
// there exist consecutive elements such that their
// subtraction differs from the one of the combination.
// colin now should know how to find the combination.
val colin: List[Choices] => List[Choices] =
  cs =>
    val colinFilter: Choices => Boolean = 
      c => uniqueSubtractions(cs)(c)
    for {
      c <- cs
      if (colinFilter(c))
    } yield {
      c
    }

val solution: Tuple2[Int, Int] => List[Choices] = 
  choicesWithSumsAndProducts andThen alice andThen betty andThen colin

val solutions: Tuple2[Int, Int] => List[Choices] =
  (n, m) =>
    (for {
      l <- 1 to n
      k <- 1 to m 
      s = solution(l, k)
      if (unique(s))
    } yield {
      ((l, k), s.head)
    }).toList.map({ case (_, s) => s })

@main def main() =

  import test.test

  test("solution(6, 2) == List(List(3, 5))") {
    solution(6, 2) == List(List(3, 5))
  }

  test("solution(16, 2) == List(List(2, 13))") {
    solution(16, 2) == List(List(2, 13))
  }

  test("solutions(16, 4) == List(List(3, 5), List(2, 13))") {
    solutions(16, 4) == List(List(3, 5), List(2, 13))
  }

  // interactive (16, 5) and others

  import scala.io.StdIn.readInt

  println(solutions(readInt, readInt))
