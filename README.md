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

Maybe you can think of other interesting properties, perhaps by drawing extra lines.

I have implemented a derivation of the property(for the symmetric point).

I also tested the correctness of the derivation and the obtained values.

```scala
sbt:specialEllipse> test
[info] Tests:
[info] - verify deriving angle between focus lines at P
[info] - deriving equation for y at P
[info] - verify deriving solution for y at P if α == π/3.0
[info] - verify deriving solution for x at P if α == π/3.0
[info] - verify deriving angle of tangent at P if α == π/3.0
[info] - P is on ellipse
[info] - angle between focus lines at P is π/3.0
[info] - angle of tangent at P is π/4.0
[info] Run completed in 98 milliseconds.
[info] Total number of tests run: 8
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 8, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
[success]
```

