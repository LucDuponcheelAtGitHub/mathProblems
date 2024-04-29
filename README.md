# Math Problems

My twin brother, Marc, often sends me an interesting math problem.

From time to time I try to solve it using `Scala` or `Haskell` code.

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

You are a notary and Ximena asks for your help with a secret code.

Ximena's intention is to have a combination lock made for that secret code with
`l` (length) integers between `1` and `m` (maximum).

Ximena tells you that she is going to arrange a meeting with Alice and Bob during which,
in the order described below (see (*)), a sequence of events will happen.

Ximena is a smart woman and Alice and Bob are not smart enough to, either determine the
secret code during the meeting, or determine it after the meeting
(if only because after the meeting they no longer remember 100% of what happened).

What happens during that meeting is recorded as video and stored in your safe in case
Ximena should die.

Ximena knows that Alice and Bob can never see each other, and you, again after the
meeting and before her death. So, if someone asks them for the secret code, they cannot,
with the information they have (see (**)), and without that video, provide that secret
code.

(*)
The following will happen during the meeting

1. Ximena says that the numbers (of the secret code) are strictly increasing
2. Ximena says the numbers are not consecutive (too easy to guess),
3. Ximena says that the numbers are uniquely determined by their product and their sum,
4. Ximena gives the sum to Alice,
5. Ximena says that, theoretically, Alice cannot be sure that Bob can determine the sum
6. Ximena gives the product to Bob,
7. Ximena says that, theoretically, Bob can determine the sum,
8. Ximena says that the increase in numbers (see (below))
  - case (1) : is the smallest possible increase,
  - case (2) : is the largest possible increase,
9. Ximena says that the information given so far uniquely determines the product and sum
 (and therefore also the numbers).

(**)
The increase of numbers is defined by the square of the Euclidean norm of successive
differences, for example, the increase of 1, 3, 5 is 2 * 2 + 2 * 2 = 4 + 4 = 8.

*Which combination locks can you ask to have made for Ximena for case (1) or case (2)*
*with a max length of `10` integers between `1` and max maximum `9`?*

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

