# Math Problems

My twin brother, Marc, often sends me an interesting math problem.

From time to time I try to solve (and/or generalize) it using `Scala` or `Haskell` code.

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
