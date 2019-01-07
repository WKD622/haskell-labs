roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) =
    let d = sqrt (b * b - 4 * a * c)
        e = 2 * a
    in ( (-b - d) / e, (-b + d) / e )

unitVec2D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec2D (a, b, c) =
    let e = sqrt(a*a + b*b + c*c)
    in (a/e, b/e, c/e)

    