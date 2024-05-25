package projecteuler.problems

import math._
import math.{BigInt => Int}

object Problem354 {
	case class Pair[A, B](_1:A, _2:B)
	def main(args: Array[String]): Unit = {
		val start = System.currentTimeMillis()
		
		var solutions = List[Pair[Int, Int]]()
		var max = Pair[scala.Int, Int](0, 0)
		var i:Int = 16
//		do {
//			//val y1 = i * i
//			val y1 = i
//			val count1 = countForY(y1)
//			max = if (count1 > max._1) (count1, y1) else max
//
//			val y2 = 3 * y1
//			val count2 = countForY(y2)
//			max = if (count2 > max._1) (count2, y2) else max
//
//			println((System.currentTimeMillis() - start)+"ms\ti: "+i+"\tmax: "+max+"\ty: "+y1+"\tcount: "+count1+"\t3y: "+y2+"\tcount: "+count2)
//
//			i *= 3
//		} while (max._1 != 450)
		
		println(max)
		
		println("runtime: "+(System.currentTimeMillis() - start)+"ms.")
	}
	
	private def countForY(y:Int): scala.Int = {
		//solutions = List[Pair[Int, Int]]()
		val maxN:Int = (sqrt(4 * y.doubleValue / 3)).longValue
		var count:scala.Int = 0
		
		for {
			n <- -maxN to maxN		// the range where det >= 0
			root <- asInt(sqrt(det(n, y).doubleValue))
			if ((n & 1) == (root & 1))
		}
		{
			if (root == 0)
				count += 1
			else
				count += 2
		}
		
		count
	}
	
	/*
	private def solveForIntegerM (n:Int, y:Int): Option[Pair[Int, Option[Int]]] = {
		
		val d = det (n, y)
		
		if (d > 0)
		{
			val rootD = asInt(sqrt(d))
			rootD match {
				case Some(int) if ((n & 1) == (int & 1))=> ((n + int)/2, (n - int)/2)
				case _ => None
			}
		}
		
		None
	}
	*/
	private def modf (d:Double): Pair[Int, Double] = {
		val intPart = d.intValue
		Pair(intPart, d - intPart)
	}
	private def isInt (d:Double): Boolean = modf(d)._2 == 0
	private def asInt (d:Double): Option[Int] = {
		modf(d) match {
			case Pair(int, frac) if frac == 0 => Some(int)
			case _ => None
		}
	}
	
	private def fn (m:Int, n:Int): Int = m*m + n*n - m*n
	
	private def det (n:Int, y:Int): Int = 4*y - 3*n*n
}
