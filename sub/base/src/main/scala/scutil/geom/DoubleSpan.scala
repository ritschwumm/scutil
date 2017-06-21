package scutil.geom

object DoubleSpan {
	val zero	= DoubleSpan(0, 0)
	
	def startSize(start:Double, size:Double):DoubleSpan	= DoubleSpan(start, size)
	def endSize(end:Double, size:Double):DoubleSpan		= DoubleSpan(end - size, size)
	def startEnd(start:Double, end:Double):DoubleSpan	= DoubleSpan(start, end - start)
}

final case class DoubleSpan(start:Double, size:Double) {
	def end:Double		= start + size
	def empty:Boolean	= size == 0
	def min:Double		= start min end
	def max:Double		= start max end
	def center:Double	= (start + end) / 2
	
	def negate:DoubleSpan	= DoubleSpan(end, -size)
	
	def move(d:Double):DoubleSpan	= DoubleSpan(start + d, size)
	def unmove(d:Double):DoubleSpan	= DoubleSpan(start - d, size)
	
	def scale(f:Double):DoubleSpan		= DoubleSpan(start * f, size * f)
	def unscale(f:Double):DoubleSpan	= DoubleSpan(start / f, size / f)

	def contains(it:Double):Boolean	=
			it >= start && it < end
		
	def normal:Boolean	= size >= 0
	def normalize:DoubleSpan	=
			if (normal)	this
			else		negate
			
	def union(that:DoubleSpan):DoubleSpan	=
			DoubleSpan startEnd (
				start	= this.start	min that.start,
				end		= this.end		min that.end
			)
			
	def rectWith(that:DoubleSpan):DoubleRect	=
			DoubleRect horizontalWithVertical (this, that)
}
