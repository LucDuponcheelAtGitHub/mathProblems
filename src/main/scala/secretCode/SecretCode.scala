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

def isLeastIncreasingAmong[Z: Numeric: Ordering]: Seq[Seq[Z]] => Seq[Z] => Boolean =
  val ordering = summon[Ordering[Z]]
  import ordering.lt
  cs => c => cs.forall(c2 => c2 == c || lt(increase apply c, increase apply c2))

def isMostIncreasingAmong[Z: Numeric: Ordering]: Seq[Seq[Z]] => Seq[Z] => Boolean =
  val ordering = summon[Ordering[Z]]
  import ordering.gt
  cs => c => cs.forall(c2 => c2 == c || gt(increase apply c, increase apply c2))

// specific

type C = Seq[Int] // Code

type M = Int // Max

type L = Int // Length

type ML = ((M, L))

val nonConsecutiveStrictlyIncreasingCodes: ML => Seq[C] =
  case (m, l) =>
    l match {
      case 0 => Seq(Seq())
      case _ =>
        (for {
          i <- 1 to m
          c <- nonConsecutiveStrictlyIncreasingCodes(m, l - 1)
          if (c.isEmpty || (i < c.head && i != c.head - 1))
        } yield {
          i +: c
        }).toSeq
    }

type S = Int // Sum

type P = Int // Product

type SP = ((S, P))

type CSP = (C, S, P)

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

val you: Seq[CSP] => Seq[CSP] =
  csps =>
    for {
      csp @ (c, l, p) <- csps
      cs = for { (c, _, _) <- csps } yield { c }
      // if ((isLeastIncreasingAmong apply cs)(c))
      if ((isMostIncreasingAmong apply cs)(c))
    } yield {
      csp
    }

lazy val nonConsecutiveStrictlyIncreasingCodesWithSumAndProduct: ML => Seq[CSP] =
  (m, l) =>
    for {
      c <- nonConsecutiveStrictlyIncreasingCodes(m, l)
      l = c.sum
      p = c.product
    } yield {
      (c, l, p)
    }

lazy val uniquelyDefinedBySumAndProduct: Seq[CSP] => Seq[CSP] =
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

val ximena =
  nonConsecutiveStrictlyIncreasingCodesWithSumAndProduct andThen
    uniquelyDefinedBySumAndProduct

val solution: ML => Seq[CSP] = ximena andThen alice andThen bob andThen you

val solutionsUpTo: ML => Seq[Seq[((M, L), CSP)]] =
  (maxM, maxL) =>
    ((for {
      m <- 1 to maxM
      l <- 1 to maxL
      csps = solution(m, l)
      if (unique(csps))
      csp = csps.head
    } yield {
      ((m, l), csp)
    }).toSeq).groupBy(_._1).values.toSeq.sorted

@main def main() =
  import scala.io.StdIn.readInt
  println(solutionsUpTo(
    { print("please type a max maximum: ") ; readInt()}, 
    { print("please type a max length: ") ; readInt()}, 
    ))
