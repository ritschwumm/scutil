package scutil.geom

object IntSpan {
	val zero	= new IntSpan(0, 0)

	def startSize(start:Int, size:Int):IntSpan	= new IntSpan(start, size)
	def endSize(end:Int, size:Int):IntSpan		= new IntSpan(end - size, size)
	def startEnd(start:Int, end:Int):IntSpan	= new IntSpan(start, end-start)
}

final class IntSpan private (val start:Int, val size:Int) {
	val end:Int			= start + size
	def empty:Boolean	= size == 0
	def min:Int			= start min end
	def max:Int			= start max end
	def center:Int		= (start + end) / 2

	def negate:IntSpan	= new IntSpan(end, -size)

	def move(d:Int):IntSpan		= new IntSpan(start + d, size)
	def unmove(d:Int):IntSpan	= new IntSpan(start - d, size)

	def scale(f:Int):IntSpan	= new IntSpan(start * f, size * f)
	def unscale(f:Int):IntSpan	= new IntSpan(start / f, size / f)

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

	def intersect(that:IntSpan):Option[IntSpan]	=
				 if (this.end   <= that.start)							None
			else if (this.start >= that.end)							None
			else if (this.start	>= that.start && this.end <= that.end)	Some(this)
			else if (this.start	<= that.start && this.end >= that.end)	Some(that)
			else if (this.start	<= that.start && this.end <= that.end)	Some(IntSpan startEnd (that.start, this.end))
			else														Some(IntSpan startEnd (this.start, that.end))

	def rectWith(that:IntSpan):IntRect	=
			IntRect horizontalWithVertical (this, that)

	def toDoubleSpan:DoubleSpan	= DoubleSpan startSize (start, size)

	//------------------------------------------------------------------------------

	override def equals(that:Any):Boolean	=
			that match {
				case that:IntSpan	=> this.start == that.start && this.size == that.size
				case _				=> false
			}

	override def hashCode():Int		= this.start ^ this.size

	override def toString:String	= s"IntSpan(start=$start, end=$end, size=$size)"
}
