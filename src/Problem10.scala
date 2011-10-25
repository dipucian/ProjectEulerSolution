import util.prime.PrimeUtil._

object Problem10 {
	def main(args: Array[String]): Unit = {
		println("Find the sum of all the primes below two million.")
		val start = System.currentTimeMillis();
		var result:Long = 0
		for (p <- allPrimes takeWhile(_ < 2000000)) result += p
		println(result)
		println("runtime: "+(System.currentTimeMillis() - start)+"ms.")
	}
}
