package secretCode

import scala.collection.immutable.Seq

// generic

def unique[Z]: Seq[Z] => Boolean =
  case Seq(_) => true
  case _       => false

def euclideanNorm[Z: Numeric]: Seq[Z] => Z =
  val numeric = summon[Numeric[Z]]
  import numeric.times
  zs => (for { z <- zs } yield { times(z, z) }).sum

def increaseSteps[Z: Numeric]: Seq[Z] => Seq[Z] =
  val numeric = summon[Numeric[Z]]
  import numeric.minus
  c =>
    (for {
      (i, index) <- c.zipWithIndex.init
      j = c(index + 1)
    } yield {
      minus(j, i)
    })

def increase[Z: Numeric]: Seq[Z] => Z =
  increaseSteps andThen euclideanNorm

def hasNonTrivialIncrease[Z: Numeric]: Seq[Z] => Boolean =
  c => (increaseSteps apply c).exists(_ != 1)

def hasSmallestIncreaseAmong[Z: Numeric: Ordering]: Seq[Seq[Z]] => Seq[Z] => Boolean =
  val ordering = summon[Ordering[Z]]
  import ordering.lteq
  cs => c => cs.forall(c2 => lteq(increase apply c, increase apply c2))

def hasLargestIncreaseAmong[Z: Numeric: Ordering]: Seq[Seq[Z]] => Seq[Z] => Boolean =
  val ordering = summon[Ordering[Z]]
  import ordering.gteq
  cs => c => cs.forall(c2 => gteq(increase apply c, increase apply c2))

// specific

type C = Seq[Int] // Code

type M = Int // Max

type L = Int // Length

type ML = ((M, L))

val codes: ML => Seq[C] =
  case (m, l) =>
    l match {
      case 0 => Seq(Seq())
      case _ =>
        (for {
          i <- 1 to m
          c <- codes(m, l - 1)
          if (c.isEmpty || (i < c.head && i != c.head - 1))
        } yield {
          i +: c
        }).toSeq
    }

type S = Int // Sum

type P = Int // Product

type SP = ((S, P))

type CSP = (C, S, P)

val ximena1: Seq[CSP] => Seq[CSP] =
  csps =>
    for {
      csp @ (_, l, p) <- csps
      if (unique(for {
        (_, s2, p2) <- csps
        if (l == s2 && p == p2)
      } yield {
        ()
      }))
    } yield {
      csp
    }

val productsForSumIn: Seq[CSP] => S => Seq[P] =
  csps =>
    s1 =>
      for {
        (c, s2, p) <- csps
        if s1 == s2
      } yield {
        p
      }

val sumsForProductIn: Seq[CSP] => P => Seq[S] =
  csps =>
    p1 =>
      for {
        (cs, l, p2) <- csps
        if p1 == p2
      } yield {
        l
      }

val sumsForProductsFromSumIn: Seq[CSP] => S => Seq[Seq[S]] =
  csps =>
    l =>
      for {
        p <- productsForSumIn(csps)(l)
      } yield {
        sumsForProductIn(csps)(p)
      }

val alice: Seq[CSP] => Seq[CSP] =
  csps =>
    for {
      csp @ (_, l, _) <- csps
      if (!sumsForProductsFromSumIn(csps)(l).forall(unique))
    } yield {
      csp
    }

val bob: Seq[CSP] => Seq[CSP] =
  csps =>
    for {
      csp @ (_, _, p) <- csps
      if (unique(sumsForProductIn(csps)(p)))
    } yield {
      csp
    }

val ximena2: Seq[CSP] => Seq[CSP] =
  csps =>
    for {
      csp @ (c, l, p) <- csps
      cs = for { (c, _, _) <- csps } yield { c }
      if ((hasSmallestIncreaseAmong apply cs)(c))
      // if ((hasLargestIncreaseAmong apply cs)(c))
    } yield {
      csp
    }

val codesWithSumAndProduct: ML => Seq[CSP] =
  (m, l) =>
    for {
      c <- codes(m, l)
      l = c.sum
      p = c.product
    } yield {
      (c, l, p)
    }

val solution: ML => Seq[CSP] =
  codesWithSumAndProduct andThen
    ximena1 andThen
    alice andThen
    bob andThen
    ximena2

val solutionsUpTo: ML => Seq[Seq[((M, L), CSP)]] =
  (maxM, maxLength) =>
    ((for {
      m <- 1 to maxM
      l <- 1 to maxLength
      csps = solution(m, l)
      if (unique(csps))
      csp = csps.head
    } yield {
      ((m, l), csp)
    }).toSeq).groupBy(_._1).values.toSeq.sorted

@main def main() =
  import scala.io.StdIn.readInt
  println(solutionsUpTo(readInt(), readInt()))
