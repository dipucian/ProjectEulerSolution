package util

object Timer {
	def time(fn: => Unit) = {
		val start = System.currentTimeMillis
		fn
		val duration = System.currentTimeMillis - start
		println(duration+"ms.")
	}
}