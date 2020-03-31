smallTriangles = [(a, b, c)| a <- [1..10], b <- [1..a], c <- [1..10]]
rightSmallTriangles = [(a, b, c) | (a, b, c) <- smallTriangles, a*a + b*b == c*c, a+b+c == 24]