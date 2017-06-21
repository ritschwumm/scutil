package scutil.geom

object IntSpan {
	val zero	= IntSpan(0, 0)
	
	def startSize(start:Int, size:Int):IntSpan	= IntSpan(start, size)
	def endSize(end:Int, size:Int):IntSpan		= IntSpan(end - size, size)
	def startEnd(start:Int, end:Int):IntSpan	= IntSpan(start, end-start)
}

final case class IntSpan(start:Int, size:Int) {
	val end:Int			= start + size
	def empty:Boolean	= size == 0
	def min:Int			= start min end
	def max:Int			= start max end
	def center:Int		= (start + end) / 2
	
	def negate:IntSpan	= IntSpan(end, -size)
	
	def move(d:Int):IntSpan		= IntSpan(start + d, size)
	def unmove(d:Int):IntSpan	= IntSpan(start - d, size)
	
	def scale(f:Int):IntSpan	= IntSpan(start * f, size * f)
	def unscale(f:Int):IntSpan	= IntSpan(start / f, size / f)
	
	def contains(it:Int):Boolean	=
			it >= start && it < end
		
	def normal:Boolean	= size >= 0
	def normalize:IntSpan	=
			if (normal)	this
			else		negate
			
	def union(that:IntSpan):IntSpan	=
			IntSpan startEnd (
				start	= this.start	min that.start,
				end		= this.end		min that.end
			)
			
	def rectWith(that:IntSpan):IntRect	=
			IntRect horizontalWithVertical (this, that)
}
