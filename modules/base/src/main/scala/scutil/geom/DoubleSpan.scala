package scutil.geom

import scutil.lang._

object DoubleSpan {
	val zero	= new DoubleSpan(0, 0)

	def startSize(start:Double, size:Double):DoubleSpan	= new DoubleSpan(start, size)
	def endSize(end:Double, size:Double):DoubleSpan		= new DoubleSpan(end - size, size)
	def startEnd(start:Double, end:Double):DoubleSpan	= new DoubleSpan(start, end - start)

	//------------------------------------------------------------------------------

	protected def apply(start:Double, size:Double):DoubleSpan	= new DoubleSpan(start, size)
}

final class DoubleSpan private (val start:Double, val size:Double) {
	def end:Double		= start + size
	def empty:Boolean	= size == 0
	def min:Double		= start min end
	def max:Double		= start max end
	def center:Double	= (start + end) / 2

	def negate:DoubleSpan	= new DoubleSpan(end, -size)

	def move(d:Double):DoubleSpan	= new DoubleSpan(start + d, size)
	def unmove(d:Double):DoubleSpan	= new DoubleSpan(start - d, size)

	def scale(f:Double):DoubleSpan		= new DoubleSpan(start * f, size * f)
	def unscale(f:Double):DoubleSpan	= new DoubleSpan(start / f, size / f)

	def contains(it:Double):Boolean	=
			it >= start && it < end

	def normal:Boolean	= size >= 0
	def normalize:DoubleSpan	=
		if (normal)	this
		else		negate

	def union(that:DoubleSpan):DoubleSpan	=
		DoubleSpan.startEnd(
			start	= this.start	min that.start,
			end		= this.end		min that.end
		)

	def intersect(that:DoubleSpan):Option[DoubleSpan]	=
			 if (this.end   <= that.start)							None
		else if (this.start >= that.end)							None
		else if (this.start	>= that.start && this.end <= that.end)	Some(this)
		else if (this.start	<= that.start && this.end >= that.end)	Some(that)
		else if (this.start	<= that.start && this.end <= that.end)	Some(DoubleSpan.startEnd(that.start, this.end))
		else														Some(DoubleSpan.startEnd(this.start, that.end))

	def rectWith(that:DoubleSpan):DoubleRect	=
		DoubleRect.horizontalWithVertical(this, that)

	def inset(start:Double, end:Double):DoubleSpan	=
		DoubleSpan.startEnd(this.start + start, this.end - end)

	def transformer(that:DoubleSpan):Endo[Double]	=
		pos => (pos - this.start) * that.size / this.size + that.start

	//------------------------------------------------------------------------------

	override def equals(that:Any):Boolean	=
		that match {
			case that:DoubleSpan	=> this.start == that.start && this.size == that.size
			case _					=> false
		}

	override def hashCode():Int		= this.start.hashCode ^ this.size.hashCode

	override def toString:String	= s"DoubleSpan(start=${start.toString}, end=${end.toString}, size=${size.toString})"
}
