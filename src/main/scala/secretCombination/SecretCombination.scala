package secretCombination

import math.Numeric.Implicits.infixNumericOps

import math.Ordering.Implicits.infixOrderingOps

def unique[Z]: List[Z] => Boolean =
  case List(z) => true
  case _       => false

def notUnique[Z]: List[Z] => Boolean = zs => !(unique(zs))

def ascending: List[Int] => Boolean =
  case Nil      => true
  case _ :: Nil => true
  case z :: zs  => ascending(zs) && z < zs.head

val choices: Int => Int => List[List[Int]] =
  n =>
    case 0 => List(List())
    case m =>
      (for {
        z <- 1 to n
        zs <- choices(n)(m - 1)
        if (ascending(z :: zs))
      } yield {
        z :: zs
      }).toList

val products: List[(List[Int], Int, Int)] => List[Int] =
  triples =>
    for {
      (zs, s, p) <- triples
    } yield {
      p
    }

val uniqueProducts: List[(List[Int], Int, Int)] => List[Int] =
  triples =>
    val theProducts = products(triples)
    for {
      p1 <- theProducts
      if unique(
        for {
          p2 <- theProducts
          if p1 == p2
        } yield {
          p2
        }
      )
    } yield {
      p1
    }

val choicesWithSumsAndProducts: Int => Int => List[(List[Int], Int, Int)] =
  n =>
    m =>
      for {
        choice <- choices(n)(m)
      } yield {
        (choice, choice.sum, choice.product)
      }

val productsForSum: Int => List[(List[Int], Int, Int)] => List[Int] =
  s1 =>
    triples =>
      for {
        (zs, s2, p) <- triples
        if s1 == s2
      } yield {
        p
      }

val sumsForProduct: Int => List[(List[Int], Int, Int)] => List[Int] =
  p1 =>
    triples =>
      for {
        (zs, s, p2) <- triples
        if p1 == p2
      } yield {
        s
      }

def subtractions: List[Int] => List[Int] =
  zs =>
    for {
      z <- zs.init
    } yield {
      zs(zs.indexOf(z) + 1) - z
    }

// you can choose between forall and exists
val subtractionsDiffer: List[Int] => List[Int] => Boolean =
  // zs1 => zs2 => subtractions(zs1).zip(subtractions(zs2)).forall(_ != _)
  zs1 => zs2 => subtractions(zs1).zip(subtractions(zs2)).exists(_ != _)

val uniqueSubtractions: List[List[Int]] => List[Int] => Boolean =
  zss2 =>
    zs1 =>
      (for {
        zs2 <- zss2
        if (zs1 != zs2)
      } yield {
        zs2
      }).forall(subtractionsDiffer(zs1))

val combinations: Int => Int => List[List[Int]] =
  n =>
    m =>
      val theChoicesWithSumsAndProducts: List[(List[Int], Int, Int)] = choicesWithSumsAndProducts(n)(m)
      val filteredChoicesWithSumsAndProducts: List[(List[Int], Int, Int)] =
        for {
          (choice, sum, product) <- theChoicesWithSumsAndProducts
          if ((for {
            product <- productsForSum(sum)(theChoicesWithSumsAndProducts)
          } yield {
            sumsForProduct(product)(theChoicesWithSumsAndProducts)
          }).exists(notUnique))
        } yield {
          (choice, sum, product)
        }
      val filteredChoicesWithUniqueSumForProduct: List[List[Int]] =
        for {
          (choice, sum, product) <- filteredChoicesWithSumsAndProducts
          if (unique(sumsForProduct(product)(filteredChoicesWithSumsAndProducts)))
        } yield {
          choice
        }
      val filteredChoicesWithUniqueSumForProductAndWithUniqueSubtractions: List[List[Int]] =
        for {
          choice <- filteredChoicesWithUniqueSumForProduct
          if (uniqueSubtractions(filteredChoicesWithUniqueSumForProduct)(choice))
        } yield {
          choice
        }
      filteredChoicesWithUniqueSumForProductAndWithUniqueSubtractions

val numbers: Int => Int => List[((Int, Int), List[Int])] =
  n =>
    m =>
      (for {
        l <- 1 to n
        k <- 1 to m
        theCombinations = combinations(l)(k)
        if (unique(theCombinations))
      } yield {
        ((l, k), theCombinations.head)
      }).toList

@main def main() =

  import test.test

  test("15 is unique product for choices(6)(2)") {
    uniqueProducts(choicesWithSumsAndProducts(6)(2)).contains(15)
  }

  test("26 is unique product for choices(16)(2)") {
    uniqueProducts(choicesWithSumsAndProducts(16)(2)).contains(26)
  }

  test("combinations(6)(2) == List(List(3, 5))") {
    combinations(6)(2) == List(List(3, 5))
  }

  test("combinations(16)(2) == List(List(2, 13))") {
    combinations(16)(2) == List(List(2, 13))
  }

  test("combinations(10)(3) == List()") {
    combinations(10)(3) == List()
  }
