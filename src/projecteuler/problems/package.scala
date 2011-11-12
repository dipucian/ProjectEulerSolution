package projecteuler

package object problems {
	def time(fn: =>Unit) = util.Timer.time(fn)
}