package projecteuler.problems

object Problem4 {

	def main(args: Array[String]): Unit = {
		val max = (for {
			i <- 999 to 1 by -1
			j <- 999 to 1 by -1
			n = i*j
			if (check (n))
		} yield n).max
		
		println(max)
	}
	
	def check(n:Int) = {
		val s = n.toString
		if (s.length == 1) true
		else {
			val front = s.substring(0, s.length/2)
			val back = s.substring((s.length+1)/2)
			front == back.reverse
		}
	}
}