package scutil.gui.geom

object SgSpan {
	//------------------------------------------------------------------------------
	//## simple values

	val zero	= startEnd(0, 0)
	val one		= startEnd(0, 1)

	//------------------------------------------------------------------------------
	//## component factory

	def startZeroBy(size:Double):SgSpan	=
		startEnd(0, size)

	def endZeroBy(size:Double):SgSpan	=
		startEnd(-size, 0)

	def startEnd(start:Double, end:Double):SgSpan	=
		new SgSpan(start, end)

	def startBy(start:Double, size:Double):SgSpan	=
		startEnd(start, start+size)

	def endBy(end:Double, size:Double):SgSpan		=
		startEnd(end-size, end)

	def centerBy(center:Double, size:Double):SgSpan	=
		startEnd(center-size/2, center+size/2)
}

final case class SgSpan private (start:Double, end:Double) {
	def empty:Boolean	= start == end
	def normal:Boolean	= start <= end
	def size:Double		= end - start
	def min:Double		= start min end
	def max:Double		= start max end
	def center:Double	= (start + end) / 2

	def swap:SgSpan	= SgSpan.startEnd(end, start)

	def normalize:SgSpan	= if (normal) this else swap

	def union(that:SgSpan):SgSpan	=
		SgSpan.startEnd(
			this.min min that.min,
			this.max max that.max
		)

	def intersect(that:SgSpan):Option[SgSpan]	=
		if		(this.empty || that.empty)							None
		else if (this.end	<= that.start)							None
		else if (this.start	>= that.end)							None
		else if (this.start	<= that.start && this.end >= that.end)	Some(that)
		else if (this.start	>= that.start && this.end <= that.end)	Some(this)
		else if (this.start	<= that.start && this.end <= that.end)	Some(SgSpan.startEnd(that.start, this.end))
		else														Some(SgSpan.startEnd(this.start, that.end))

	// TODO should this normalize?
	def containsValue(pos:Double) = {
		val	normal	= normalize
		pos >= normal.start && pos < normal.end
	}

	// TODO should this normalize?
	def contains(that:SgSpan):Boolean	= {
		val thisNormal	= this.normalize
		val thatNormal	= this.normalize
		thatNormal.start	>=	thisNormal.start	&&
		thatNormal.start	<	thisNormal.end		&&
		thatNormal.end		<=	thisNormal.end		&&
		thatNormal.end		>	thisNormal.start
	}

	// TODO should this normalize?
	def intersects(that:SgSpan):Boolean	= {
		if (this.empty || that.empty) {
			false
		}
		else {
			val thisNormal	= this.normalize
			val thatNormal	= that.normalize
			thatNormal.start < thisNormal.end && thatNormal.end > thisNormal.start
		}
	}

	def inset(insets:SgSpanInsets):SgSpan	=
		SgSpan.startEnd(
			start	+ insets.start,
			end		- insets.end
		)

	def move(offset:Double):SgSpan	=
		SgSpan.startEnd(
			start	+ offset,
			end		+ offset
		)

	def splitAt(position:Double):(SgSpan, SgSpan)	=
		(
			SgSpan.startEnd(start, position),
			SgSpan.startEnd(position, end)
		)

	def splitStartBy(size:Double):(SgSpan, SgSpan)	=
		splitAt(start + size)

	def splitEndBy(size:Double):(SgSpan, SgSpan)	=
		splitAt(end - size)

	//------------------------------------------------------------------------------
	//## more span dsl

	def previous(size:Double):SgSpan	= SgSpan.startEnd(start - size,	start)
	def next(size:Double):SgSpan		= SgSpan.startEnd(end,			end   + size)
	def starting(size:Double):SgSpan	= SgSpan.startEnd(start,		start + size)
	def ending(size:Double):SgSpan		= SgSpan.startEnd(end   - size,	end)

	//------------------------------------------------------------------------------
	//## factory dsl

	def lineWith(that:SgSpan):SgLine	=
		SgLine.startEnd(
			SgPoint(this.start, that.start),
			SgPoint(this.end, that.end)
		)

	def rectangleWith(that:SgSpan):SgRectangle	=
		SgRectangle.xy(this, that)

	def linearTransformTo(that:SgSpan):SgLinearTransform1D	=
		SgLinearTransform1D.fromTo(this, that)
}
