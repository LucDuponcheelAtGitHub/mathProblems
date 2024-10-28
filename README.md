# Math Problems

My twin brother, Marc, often sends me an interesting math problem.

From time to time I try to solve (and/or generalize) it using `Scala` or `Haskell` code.

## Special ellipse

Consider the following
[picture](https://github.com/LucDuponcheelAtGitHub/mathProblems/blob/master/png/ellipse.png).

The ratio of the lenghts of the sides of the blue rectangle is `sqrt(2)` (cfr. A4 paper).

The size of the angle between the two green lines, through the point on the ellipse
and the two focuses of the ellipse, is, as indicated, `π/3` (`60` degrees).

*What is size of the angle between the green slope through the point on the*
*ellipse and the horizontal line (not shown) through the focuses of the ellipse?*

The answer, as you can guess from the picture, is `π/4` (`45` degrees`).

*Can you derive the property?*

The same property holds, of course, for the point (not shown) on the ellipse
which is symmetric w.r.t. the center (not shown) of the ellipse.

Many other interesting properties follow from this property.

Maybe you can think of other interesting properties,
perhaps by drawing extra lines or circles.

I have implemented a derivation of the property (for the symmetric point).

I also tested the correctness of the derivation and the obtained values.

## Secret combination

Ximena has a combination lock with`l` (length) integers between `1` and `m` (maximum).
Ximena also has a secret code consisting of non consecutive strictly increasing numbers.
Moreover the numbers are uniquely defined by their sum and product.
Ximena gives the sum to Alice and the product to Bob.
Alice resp. Bob knows that Bob resp. Alice knows the product resp. sum.
Knowing the sum, Alice, a clever women, tells Ximena that she cannot deduce that Bob can deduce the sum.
Knowing the product, Bob, a clever man, tells Ximena that he can deduce the sum.
Ximana tells you the story above and also tells you that the numbers are
case (1) : the unique least increasing ones,
case (2) : the unique most increasing ones
among the ones that you, as a clever person, can deduce so far.

The increase of numbers is defined by the square of the Euclidean norm of successive
differences, for example, the increase of 1, 3, 5 is 2 * 2 + 2 * 2 = 4 + 4 = 8.

*Which secret codes can Ximena have for case (1) or case (2) for a combination lock*
*with a max length of `9` integers between `1` and a max maximum of `9`?*

*Answer:* 

notation `(((maximum,length),(secret code,sum,product)))`

- case (1)
  - `(((8,3),((3, 5, 7),15,105)))`
  - `(((9,2),((4, 7),11,28)))`
  - `(((9,3),((3, 5, 7),15,105)))`

- case (2)
  - `(((8,2),((2, 8),10,16))`
  - `(((8,3),((1, 3, 7),11,21)))`
  - `(((9,2),((1, 9),10,9)))`
  - `(((9,3),((1, 5, 9),15,45)))`

## Matrix puzzles

Matrix puzzles, like sudoku, are puzzles where, somehow,
unique values need to be provided.

Sudoku is a symbolic matrix puzzle, traditionally, the symbols are
'1', '2', '3', '4', 5', '6', '7', '8' and '9', but they can also be
'a', 'b', 'c', 'd', e', 'f', 'g', 'h' and 'i'.

Below is another matrix puzzle, this time a numeric matix puzzle.

Complete the triangle below with unique natural numbers between 1 and 8
for the placeholders '?'
such that the products of all sides of the triangle are equal.

```
          8
      ?       ?
  2       ?       6
```

Below are two ways to represent the triangle above as a matrix

- as a matrix of sides

```
8 ? 2
2 ? 6
6 ? 8
```

- as a matrix of initial parts of sides

```
8 ?
2 ?
6 ?
```

The solution of the puzzle is

```
          8
      3       1
  2       4       6
```

The puzzle above is a specific 3, 3, 8 instance of a generic z, y, z puzzle, where
z is the amount of vertices of a polygon,
y the amount of natural numbers on each vertex, and,
x is the maximum natural number.

The code is based upon https://www.cs.nott.ac.uk/~pszvc/g52afp/sudoku.lhs,

## Alice splits the bill not too generously with Bob

My twin brother, Marc, published this interesting puzzle on
[PuzzlingStackExchange](https://puzzling.stackexchange.com/questions/128395/alice-splits-the-bill-not-too-generously-with-bob).

I found it challenging use to `forever` (in fact just the standard `iterate` but I added it for clarity) to build a
lazy infinite *puzzle triangle* similar to the well known *pascal triangle*.

for example, similar to the pascal triangle

```haskell
0
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1
```

or, annotated with coordinates,

```haskell
((0,0),0)
((1,0),1) ((0,1),1)
((2,0),1) ((1,1),2) ((0,2),1)
((3,0),1) ((2,1),3) ((1,2),3) ((0,3),1)
((4,0),1) ((3,1),4) ((2,2),6) ((1,3),4) ((0,4),1)
((5,0),1) ((4,1),5) ((3,2),10) ((2,3),10) ((1,4),5) ((0,5),1)
```

the annotated puzzling triangle looks like

```haskell
((0,0),(0 % 1,0.0))
((1,0),(1 % 1,1.0)) ((0,1),(1 % 1,1.0))
((2,0),(2 % 1,2.0)) ((1,1),(3 % 2,1.5)) ((0,2),(2 % 1,2.0))
((3,0),(3 % 1,3.0)) ((2,1),(7 % 3,2.3333333333333335)) ((1,2),(7 % 3,2.3333333333333335)) ((0,3),(3 % 1,3.0))
((4,0),(4 % 1,4.0)) ((3,1),(13 % 4,3.25)) ((2,2),(17 % 6,2.8333333333333335)) ((1,3),(13 % 4,3.25)) ((0,4),(4 % 1,4.0))
((5,0),(5 % 1,5.0)) ((4,1),(21 % 5,4.2)) ((3,2),(18 % 5,3.6)) ((2,3),(18 % 5,3.6)) ((1,4),(21 % 5,4.2)) ((0,5),(5 % 1,5.0))
```

where `(3 % 2,1.5)` is the *on average, correctly guessed percentage of bit values*, both as a rational number
and as a real number.

Using the triangle the integer `42` (following `41`) leading to the solution of the puzzle can be found extremely fast.

```haskell
$ time ./puzzle128395 
((59,41),(6114634921008473035429070497 % 100582201066849840253175876,60.7924151206883))

real    0m0.053s
user    0m0.029s
sys     0m0.015s
```

As a bonus I also programmed `fibonacci` using `forever`.

## From the 2011 South African Junior Olympiad

My twin brother, Marc, pointed me at this interesting puzzle on
[PuzzlingStackExchange](https://puzzling.stackexchange.com/questions/128595/prove-that-it-is-always-possible-for-them-to-join-the-line-so-that-the-number-of).

The solution is an amazingly simple one.

The correct position is the one with "the amount of women in the line" persons before him.

I found it challenging to use `fixedPointOf`, defined using `until` (in fact a rewritten version of the standard
`until`), to let the latecomer move from an arbitrary position to the correct position.

I also gave a proof of the correctnes of the correct position (with many thanks to my twin brother, Marc, for pointing
out some inaccuracies in the proof).

## Reserved seat occupation

**Question**

100 passengers take their `100` reserved, typically labeled, seats on a plane with exactly `100` (for this problem,
identical) seats, one by one. The first passenger is confused and may have lost his boarding pass, and chooses a random
seat. The next passengers are alert and have their boarding passes and proceed as follows: if their reserved seat is
free, then they take that seat, if not, they choose a random free seat. What is the probability that the last passenger
ends up in the seat reserved for that passenger?

**Answer**

The answer generalizes `100` to `n`.

Let's call the probability `p n`

Clearly `p 2 = 1/2`.

The `1`st passenger takes his seat with probability `1/n`. The passenger whose seat is already occupied by the `1`st
passenger can, for every `m <- [2..n]`, with probability `1/(n-1)`, take a seat as `m`th passenger. If he takes a seat
as `n`th passenger, then he cannot sit at his seat, so only `m <- [2..(n-1)]` needs to be considered. The answer then
reduces `n-1` times, recursively, to `p (n-(m-1))`.

Below is `Haskell` code.

```haskell
p 2 = 1/2
p n = (1 + sum [ p (n-(m-1)) | m <- [2..(n-1)] ]) / fromIntegral n
```

It is now easy to prove that `p n = 1/2` for all `n`.

```haskell
p n = (1 + sum [ p (n-(m-1)) | m <- [2..(n-1)] ]) / n
    = (1 + sum [ 1/2 | m <- [2..(n-1)] ]) / n -- induction
    = (1 + (n-2)/2) / n 
    = (2 + (n-2)) / (2*n) 
    = n / (2*n) 
    = 1/2 
```