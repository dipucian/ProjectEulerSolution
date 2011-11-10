package util.prime

import scala.math._

object PrimeUtil {

	val allPrimes = primes()
	def primes(start: Int = 2): Stream[Long] = {
		if (start < 8)
			return 2 #:: 3 #:: 5 #:: 7 #:: primes(11)

		var n = if (start % 2 == 0) start + 1 else start
		while (!isPrime(n))
			n += 2
		n #:: primes(n + 2)
	}

	def isPrime(n: Long): Boolean = {
		if (n == 1) return false
		else if (n < 4) return true // 2 and 3 are primes

		val rootN = sqrt(n).toLong
		for (p <- allPrimes.takeWhile(_ <= rootN))
			if (n % p == 0) return false
		return true
	}

	def primeFactorize(n: Long): List[(Long, Long)] = {

		var list: List[(Long, Long)] = Nil

		var running = n
		for (p <- allPrimes) {
			var root = sqrt(running).toLong
			if (p > root) {
				return (running, 1L) :: list
			}

			var count = 0
			var high = 1L
			var div = p
			while (running % div == 0) {
				count += 1
				high = div
				div *= p
			}

			if (count > 0) {
				list ::= (p, count)
				if (running == high) return list
				running /= high
			}
		}

		Nil
	}

	def pairFactorize(n: Long): Seq[(Long, Long)] = {
		val pfs = primeFactorize(n)
		val preMulti = pfs map {
			case (p, m) =>
				var y = 1L
				for (i <- 0L to m) yield {
					val t = y
					y *= p
					t
				}
		}

		def generatePair(ls: List[IndexedSeq[Long]]): Seq[(Long, Long)] = {
			ls match {
				case Nil => Seq((1L, 1L))
				case head :: tail =>
					val l = head.length
					for {
						i <- 0 to l / 2
						res <- generatePair(tail)
					} yield (head(i) * res._1, head(l - i - 1) * res._2)
			}
		}

		generatePair(preMulti)
	}

	def isPrimeGenerator(n: Long): Boolean = {
		if (n == 1) true
		else if (n % 2 == 1 || n % 4 == 0)
			// if n is odd, n + 1 must not be prime
			// if n % 4 == 0, n = 2xa x 2xb, 2xa + 2xb must not be prime
			false
		else if (isPrime(n + 1))
			pairFactorize(n).tail forall { pair => isPrime(pair._1 + pair._2) }
		else
			false
	}
}

object PrimeUtilTest {
	import PrimeUtil._

	def main(args: Array[String]) {

		val n = 4
		println("primeFactorize(" + n + "): " + primeFactorize(n))
		println("pair(" + n + "): " + pairFactorize(n))
		println("isPrimeGen(" + n + "): " + isPrimeGenerator(n))

		val pfs = primeFactorize(100)
		val range = pfs map {
			_ match {
				case (p, m) => (p, 0L to m)
			}
		}

		val preMulti = pfs map {
			_ match {
				case (p, m) =>
					var y = 1L
					for {
						i <- 0L to m
					} yield {
						val t = y
						y *= p
						t
					}
			}
		}
		println(preMulti)
		println(range)

		def fn4(ls: List[IndexedSeq[Long]]): Seq[(Long, Long)] = {
			ls match {
				case Nil => Seq((1L, 1L))
				/*case head::Nil =>
					val l = head.length - 1
					for {
						i <- 0 to l / 2
					} yield (head(i), head(l-i))
					*/
				case head :: tail =>
					val l = head.length
					for {
						i <- 0 to l / 2
						res <- fn4(tail)
					} yield {
						(head(i) * res._1, head(l - i - 1) * res._2)
					}
			}
		}
		println("fn4(preMulti): " + fn4(preMulti))

		def fn(ls: List[Seq[Long]]): Seq[Long] = {
			ls match {
				case head :: Nil => head
				case head :: tail =>
					head map { h => fn(tail) map { t => t * h } } flatten
			}
		}

		def fn2(ls: List[Seq[Long]]): Seq[Seq[Long]] = {
			ls match {
				case head :: tail =>
					head map { h => fn(tail) map { t => t * h } }
				case _ => ls
			}
		}

		def fn3(ls: List[(Long, Seq[Long])]) = {
			ls.unzip match {
				case (ps, ranges) =>

			}

			ls
		}

		println(fn(preMulti))
		println(fn2(preMulti))
		println(fn3(range))

		testPrimeFactorize()
	}

	def testPrimeFactorize() {

		println("testing primeFactorize...")

		for {
			i <- 1 to 1000000
		} {
			val pfs = primeFactorize(i)
			val orig = pfs map {
				_ match {
					case (p, m) => math.pow(p, m)
				}
			} product

			assert(orig.toInt == i, i + ": " + pfs)
		}
	}
}