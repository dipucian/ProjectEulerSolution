package projecteuler.problems
import scala.annotation.tailrec

object Problem8 {
	def main(args: Array[String]): Unit = {
		val orig = List(
			"73167176531330624919225119674426574742355349194934",
			"96983520312774506326239578318016984801869478851843",
			"85861560789112949495459501737958331952853208805511",
			"12540698747158523863050715693290963295227443043557",
			"66896648950445244523161731856403098711121722383113",
			"62229893423380308135336276614282806444486645238749",
			"30358907296290491560440772390713810515859307960866",
			"70172427121883998797908792274921901699720888093776",
			"65727333001053367881220235421809751254540594752243",
			"52584907711670556013604839586446706324415722155397",
			"53697817977846174064955149290862569321978468622482",
			"83972241375657056057490261407972968652414535100474",
			"82166370484403199890008895243450658541227588666881",
			"16427171479924442928230863465674813919123162824586",
			"17866458359124566529476545682848912883142607690042",
			"24219022671055626321111109370544217506941658960408",
			"07198403850962455444362981230987879927244284909188",
			"84580156166097919133875499200524063689912560717606",
			"05886116467109405077541002256983155200055935729725",
			"71636269561882670428252483600823257530420752963450")

		println("Find the greatest product of five consecutive digits in the 1000-digit number.")
		orig foreach println
		println

		import util.Timer.time
		
		time {
			val digitStr = orig.mkString.split("0").filter(_.length >= 5)
			println("Answer: "+ (digitStr map fiveDigitMax).max)
		}
	}
	
	def fiveDigitMax(s: String): Int = {
		import scala.collection.mutable.Queue
		
		val pair = s.splitAt(5)
		val zero = '0'.toInt
		val queue = new Queue ++= pair._1.map(_.toInt - zero)
		
		var max = queue.product
		
		for {
			c <- pair._2
			n = c.toInt - zero
		}
		{
			val head = queue.dequeue
			if (n < head) {
				val m = head * queue.product
				if (m > max) max = m
			}
			queue.enqueue(n)
		}
		
		max
	}
	
	def digitize(l: Long): Vector[Byte] = {

		@tailrec
		def extractLastDigit(l: Long, acc: Vector[Byte]): Vector[Byte] = {
			if (l < 10) l.toByte +: acc
			else {
				val mod = (l % 10).toByte
				extractLastDigit(l / 10, mod +: acc)
			}
		}

		extractLastDigit(l, Vector.empty)
	}
}
