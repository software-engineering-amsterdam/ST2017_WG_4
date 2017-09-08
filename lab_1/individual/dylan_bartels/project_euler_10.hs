-- Project Euler 10. time: 5 minutes, answer: 142913828922

sumPrimes :: Integer
sumPrimes = sum $ takeWhile (<2000000) primes
