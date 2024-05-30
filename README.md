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

          8
      ?       ?
  2       ?       6

Below are two ways to represent the triangle above as a matrix

1) as a matrix of sides

8 ? 2
2 ? 6
6 ? 8

2) as a matrix of initial parts of sides ()

8 ?
2 ?
6 ?

The solution of the puzzle is

          8
      3       1
  2       4       6

The puzzle above is a specific 3, 3, 8 instance of a generic z, y, z puzzle, where
z is the amount of vertices of a polygon,
y the amount of natural numbers on each vertex, and,
x is the maximum natural number.

The code is based upon https://www.cs.nott.ac.uk/~pszvc/g52afp/sudoku.lhs,

