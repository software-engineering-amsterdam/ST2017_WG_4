-- Project Euler 9. Time: 10 minutes, answer: (200,375,425)

pythagoreanTriplet :: (Int, Int, Int)
pythagoreanTriplet = head [(a,b,c) | c <- [1..1000], b <- [1..c], a <- [1..b],
                                     a^2+b^2 == c^2, a+b+c == 1000]
