package scutil.gui.geom

import java.awt.Insets

import scala.math.*

object SgRectangleInsets {
	//------------------------------------------------------------------------------
	//## simple values

	val zero	= symmetric(SgSpanInsets.zero)
	val one		= symmetric(SgSpanInsets.one)

	//------------------------------------------------------------------------------
	//## component factory

	def symmetric(size:SgSpanInsets):SgRectangleInsets	= xy(size, size)

	def symmetric2(size:Double):SgRectangleInsets		=
		xy(
			SgSpanInsets.symmetric(size),
			SgSpanInsets.symmetric(size)
		)

	def xy(x:SgSpanInsets, y:SgSpanInsets):SgRectangleInsets	= new SgRectangleInsets(x, y)

	//------------------------------------------------------------------------------
	//## orientation factory

	def orientationWith(orientation:SgOrientation, master:SgSpanInsets, slave:SgSpanInsets):SgRectangleInsets	=
		orientation match {
			case SgOrientation.Horizontal	=> xy(master, slave)
			case SgOrientation.Vertical		=> xy(slave, master)
		}

	//------------------------------------------------------------------------------
	//## awt conversion

	def fromAwtInsets(it:Insets):SgRectangleInsets	=
		xy(
			SgSpanInsets.startEnd(it.left,	it.right),
			SgSpanInsets.startEnd(it.top,	it.bottom)
		)

	def toAwtInsets(it:SgRectangleInsets):Insets	=
		it.toAwtInsets
}

final case class SgRectangleInsets private (x:SgSpanInsets, y:SgSpanInsets) {
	def top:Double		= x.start
	def bottom:Double	= x.end
	def left:Double		= y.start
	def right:Double	= y.end

	def empty:Boolean	= x.empty && y.empty
	def size:SgPoint	= SgPoint(x.size, y.size)

	def swap:SgRectangleInsets		= SgRectangleInsets.xy(y, x)
	def inverse:SgRectangleInsets	= SgRectangleInsets.xy(x.inverse, y.inverse)

	def +(that:SgRectangleInsets):SgRectangleInsets	=
		SgRectangleInsets.xy(
			this.x	+ that.x,
			this.y	+ that.y
		)

	def -(that:SgRectangleInsets):SgRectangleInsets	=
		SgRectangleInsets.xy(
			this.x	- that.x,
			this.y	- that.y
		)

	def *(scale:Double):SgRectangleInsets	=
		SgRectangleInsets.xy(
			x	* scale,
			y	* scale
		)

	def /(scale:Double):SgRectangleInsets	=
		SgRectangleInsets.xy(
			x	/ scale,
			y	/ scale
		)

	//------------------------------------------------------------------------------
	//## orientation lens

	def get(orientation:SgOrientation):SgSpanInsets	=
		orientation match {
			case SgOrientation.Horizontal	=> x
			case SgOrientation.Vertical		=> y
		}

	def set(orientation:SgOrientation, it:SgSpanInsets):SgRectangleInsets	=
		orientation match {
			case SgOrientation.Horizontal	=> SgRectangleInsets.xy(it, y)
			case SgOrientation.Vertical		=> SgRectangleInsets.xy(x, it)
		}

	def modify(orientation:SgOrientation, it:SgSpanInsets=>SgSpanInsets):SgRectangleInsets	=
		orientation match {
			case SgOrientation.Horizontal	=> SgRectangleInsets.xy(it(x), y)
			case SgOrientation.Vertical		=> SgRectangleInsets.xy(x, it(y))
		}

	//------------------------------------------------------------------------------
	//## awt conversion

	def toAwtInsets:Insets	=
		new Insets(
			round(top).toInt,
			round(left).toInt,
			round(bottom).toInt,
			round(right).toInt
		)
}
