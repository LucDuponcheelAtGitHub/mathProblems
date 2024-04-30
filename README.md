# Math Problems

My twin brother, Marc, often sends me an interesting math problem.

From time to time I try to solve it (and/or generalize) using `Scala` or `Haskell` code.

## Special Ellipse

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

## Secret code

Ximena has a combination lock with `l` (length) integers between `1` and `m` (maximum)
and has a secret code for it consisting of non-consecutive strictly increasing numbers.
Moreover the numbers are uniquely determined by their product and their sum.

Ximena says to Alice and Bob that she will give the sum to Alice and the product to Bob.

First, Ximena does what she promised (giving the sum to Alice and the product to Bob)
and tells them about the combination lock and the kind of increasing numbers of the
secret code.

Next, Alice says to Bob that she is not sure that Bob can determine the sum.

Next, Bob says to Alice that he already could determine the sum before you spoke.

Ximena tells you the whole story and also tells you, that the numbers are
(case (1)) the unique least increasing ones
or 
(case (2)) the unique most increasing ones 
of the possible ones you can deduce so far.

The increase of numbers is defined by the square of the Euclidean norm of successive
differences, for example, the increase of 1, 3, 5 is 2 * 2 + 2 * 2 = 4 + 4 = 8.

*Which combination locks with secret codes can you find for case (1) or case (2)*
*with a max length of `9` integers all being between `1` and max maximum value `9`?*

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

