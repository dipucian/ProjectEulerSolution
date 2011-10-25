package util.prime

import scala.math._

object PrimeUtil {

	val allPrimes = primes()
	def primes(start: Int = 2): Stream[Long] = {
		if (start < 8)
			return 2 #:: 3 #:: 5 #:: 7 #:: primes(11)
		
		var n = start
		while (!isPrime(n))
			n += 1
		n #:: primes(n + 2)
	}

	def isPrime(n: Long): Boolean = {
		if (n == 1) return false
		else if (n < 4) return true			// 2 and 3 are primes
		
		val rootN = sqrt(n).toLong
		for (p <- allPrimes.takeWhile(_ <= rootN))
			if (n % p == 0) return false
		return true
	}
}