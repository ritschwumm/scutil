package scutil.geom

import java.lang.Math.*
import java.lang.Integer.{ signum as isignum }

object IntPoint {
	val zero	= IntPoint(0, 0)
	val one		= IntPoint(1, 1)
}

final case class IntPoint(x:Int, y:Int) {
	def unary_- :IntPoint	= negate
	def unary_~ :IntPoint	= swap

	def + (that:IntPoint):IntPoint	= add(that)
	def - (that:IntPoint):IntPoint	= sub(that)

	def * (that:IntPoint):IntPoint	= mul(that)
	def / (that:IntPoint):IntPoint	= div(that)

	def *!(f:Int):IntPoint	= scale(f)
	def /!(f:Int):IntPoint	= unscale(f)

	//------------------------------------------------------------------------------

	// additive inverse
	def negate:IntPoint		= IntPoint(-x,	-y)
	// diagonal mirror
	def swap:IntPoint		= IntPoint(y,	x)

	def add(that:IntPoint):IntPoint	= IntPoint(this.x + that.x,	this.y + that.y)
	def sub(that:IntPoint):IntPoint	= IntPoint(this.x - that.x,	this.y - that.y)

	def mul(that:IntPoint):IntPoint	= IntPoint(this.x * that.x,	this.y * that.y)
	def div(that:IntPoint):IntPoint	= IntPoint(this.x / that.x,	this.y / that.y)

	def scale(f:Int):IntPoint	= IntPoint(x * f, y * f)
	def unscale(f:Int):IntPoint	= IntPoint(x / f, y / f)

	def signum:IntPoint	= IntPoint(isignum(x), isignum(y))

	//------------------------------------------------------------------------------

	def rectTo(that:IntPoint):IntRect	= IntRect	.topLeftToBottomRight	(this, that)
	def rectSize(that:IntPoint):IntRect	= IntRect	.topLeftWithSize		(this, that)

	//------------------------------------------------------------------------------

	def angle:Double	= atan2(y, x)

	// TODO this is dangerous, converting Long to Double is lossy
	def length:Double	= sqrt(lengthQ.toDouble)
	def lengthQ:Long	= x*x + y*y

	//------------------------------------------------------------------------------

	def toDoublePoint:DoublePoint	= DoublePoint(x, y)
}
