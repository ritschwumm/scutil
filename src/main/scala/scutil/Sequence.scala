package scutil

final class Sequence(initial:Long) {
	def this() { this(0L) }
	
	private var id	= initial
	
	/** the id to be issued on the next call to #next */
	def peek:Long = id
	
	/** produce a new id */
	def next():Long = {
		val	out	= id
		id	= id + 1
		out
	}
	
	def reset() { 
		id = 0 
	}
}
