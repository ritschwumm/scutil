package scutil.gui.geom

object SgSpanInsets {
	//------------------------------------------------------------------------------
	//## simple values

	val zero	= symmetric(0)
	val one		= symmetric(1)

	//------------------------------------------------------------------------------
	//## component factory

	def symmetric(size:Double):SgSpanInsets	=
		startEnd(size, size)

	def startEnd(start:Double, end:Double):SgSpanInsets	= new SgSpanInsets(start, end)
}

final case class SgSpanInsets private (start:Double, end:Double) {
	def empty:Boolean	= start == 0 && end == 0
	def size:Double		= start + end

	def inverse:SgSpanInsets	= SgSpanInsets.startEnd(-start, -end)
	def swap:SgSpanInsets		= SgSpanInsets.startEnd(end, start)

	def +(that:SgSpanInsets):SgSpanInsets	=
		SgSpanInsets.startEnd(
			this.start	+ that.start,
			this.end	+ that.end
		)

	def -(that:SgSpanInsets):SgSpanInsets	=
		SgSpanInsets.startEnd(
			this.start	- that.start,
			this.end	- that.end
		)

	def *(scale:Double):SgSpanInsets	=
		SgSpanInsets.startEnd(
			start	* scale,
			end		* scale
		)

	def /(scale:Double):SgSpanInsets	=
		SgSpanInsets.startEnd(
			start	/ scale,
			end		/ scale
		)

	//------------------------------------------------------------------------------
	//## factory dsl

	def rectangleInsetsWith(that:SgSpanInsets):SgRectangleInsets	=
		SgRectangleInsets.xy(this, that)
}
