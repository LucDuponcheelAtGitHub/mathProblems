# Math Problems

My twin brother, Marc, often sends me an interesting math problem.

From time to time I try to solve it using Scala code as a tool.

## Special Ellipse

Consider the following [picture](https://github.com/LucDuponcheelAtGitHub/mathProblems/blob/master/png/ellipse.png).

The ratio of the lenghts of the sides of the blue rectangle is `sqrt(2)` (cfr. A4 paper).

The size of the angle between the green lines, through the point on the ellipse
and a focuses of the ellipse, is, as indicated, `π/3` (`60` degrees).

*What is size of the angle between the green slope through the point on the*
*ellipse and the horizontal line (not shown) through the focuses of the ellipse?*

The answer, as you can guess from the picture, is `π/4` (`45` degrees`).

*Can you derive the property?*

The same property holds, of course, for the point (not shown) on the ellipse
which is symmetric w.r.t. the center (not shown) of the ellipse.

Many other interesting properties follow from this property.

For example: the lenghts of the two blue dashed lines are equal.

Maybe you can think of other interesting properties,
perhaps by drawing extra lines or circles.

I have implemented a derivation of the property(for the symmetric point).

I also tested the correctness of the derivation and the obtained values.

For the moment only `secretCombination.main` and `specialEllipse.main` are involved.

```scala
sbt:mathProblems> run

Multiple main classes detected. Select one to run:
 [1] secretCombination.main
 [2] specialEllipse.main

Enter number: 2
[info] running specialEllipse.main 
deriving angle between focus lines at P is OK
deriving equation for y at P is OK
deriving solution for y at P if α == π/3.0 is OK
deriving solution for x at P if α == π/3.0 is OK
deriving angle of tangent at P if α == π/3.0 is OK
P is on ellipse is OK
angle between focus lines at P is π/3.0 is OK
angle of tangent at P is π/4.0 is OK
[success]
```

and


```scala
sbt:mathProblems> run

Multiple main classes detected. Select one to run:
 [1] secretCombination.main
 [2] specialEllipse.main

Enter number: 1
[info] running secretCombination.main 
solution(6, 2) == List(List(3, 5)) is OK
solution(16, 2) == List(List(2, 13)) is OK
solutions(16, 4) == List(List(3, 5), List(2, 13)) is OK
16
5
List(List(3, 5), List(2, 13))
[success]
```

