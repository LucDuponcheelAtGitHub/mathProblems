package secretCode

import scala.collection.immutable.Seq

// generic def's

def isUnique[Z]: Seq[Z] => Boolean =
  case Seq(_) => true
  case _      => false

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

// specific type's

type C = Seq[Int] // Code

type M = Int // Max

type L = Int // Length

type ML = ((M, L)) // Max and Length

type S = Int // Sum

type P = Int // Product

type SP = ((S, P)) // Sum and Product

type CSP = (C, S, P) // Code, Sum and Product

// specific val's

val nonConsecutiveStrictlyIncreasingCodes: ML => Seq[C] =
  (m, l) =>
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

val nonConsecutiveStrictlyIncreasingCodesWithSumAndProduct: ML => Seq[CSP] =
  (m, l) =>
    for {
      c <- nonConsecutiveStrictlyIncreasingCodes(m, l)
      s = c.sum
      p = c.product
    } yield {
      (c, s, p)
    }

val isUniquelyDefinedBySumAndProductFilter: Seq[CSP] => Seq[CSP] =
  csps =>
    val sumAndProductIsUnique: (S, P) => Boolean =
      (s, p) =>
        isUnique(for {
          (_, s2, p2) <- csps
          if (s == s2 && p == p2)
        } yield {
          ()
        })
    for {
      csp @ (_, s, p) <- csps
      if (sumAndProductIsUnique(s, p))
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
        (cs, s, p2) <- csps
        if p1 == p2
      } yield {
        s
      }

val sumsForProductsFromSumIn: Seq[CSP] => S => Seq[Seq[S]] =
  csps =>
    s =>
      for {
        p <- productsForSumIn(csps)(s)
      } yield {
        sumsForProductIn(csps)(p)
      }

val ximena: ML => Seq[CSP] =
  nonConsecutiveStrictlyIncreasingCodesWithSumAndProduct andThen
    isUniquelyDefinedBySumAndProductFilter
      
val alice2ximena: Seq[CSP] => Seq[CSP] =
  csps =>
    for {
      csp @ (_, s, _) <- csps
      if (!sumsForProductsFromSumIn(csps)(s).forall(isUnique))
    } yield {
      csp
    }

val bob2ximena: Seq[CSP] => Seq[CSP] =
  csps =>
    for {
      csp @ (_, _, p) <- csps
      if (isUnique(sumsForProductIn(csps)(p)))
    } yield {
      csp
    }

val alice2you: Seq[CSP] => Seq[CSP] =
  csps =>
    for {
      csp @ (c, _, _) <- csps
      cs = for { (c, _, _) <- csps } yield { c }
      // if ((isLeastIncreasingAmong apply cs)(c))
      if ((isMostIncreasingAmong apply cs)(c))
    } yield {
      csp
    }

val solution: ML => Seq[CSP] =
  ximena andThen alice2ximena andThen bob2ximena andThen alice2you

val solutionsUpTo: ML => Seq[Seq[((M, L), CSP)]] =
  (maxM, maxL) =>
    ((for {
      m <- 1 to maxM
      l <- 1 to maxL
      csps = solution(m, l)
      if (!csps.isEmpty)
      csp = csps.head
    } yield {
      ((m, l), csp)
    }).toSeq).groupBy(_._1).values.toSeq.sorted

@main def main() =
  import scala.io.StdIn.readInt
  val maxM = { print("please type a max maximum: "); readInt() }
  val maxL = { print("please type a max length: "); readInt() }
  println(solutionsUpTo(maxM, maxM))
  // val m = { print("please type a maximum: ") ; readInt() } // 15
  // val l = { print("please type a length: ") ; readInt() } // 3
  // val seqA = nonConsecutiveStrictlyIncreasingCodesWithSumAndProduct(m, l)
  // val seqB = ximena(m, l)
  // val seqC = seqA.filter(a => !seqB.contains(a))
  // println(seqC)
