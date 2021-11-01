package scutil.geom

import scala.{ math => smath }

object DoublePoint {
	val zero:DoublePoint	= DoublePoint(0, 0)
	val one:DoublePoint		= DoublePoint(1, 1)
}

final case class DoublePoint(x:Double, y:Double) {
	def unary_- :DoublePoint	= negate
	def unary_! :DoublePoint	= inverse
	def unary_~ :DoublePoint	= swap

	def + (that:DoublePoint):DoublePoint	= add(that)
	def - (that:DoublePoint):DoublePoint	= sub(that)

	def * (that:DoublePoint):DoublePoint	= mul(that)
	def / (that:DoublePoint):DoublePoint	= div(that)

	def *!(f:Double):DoublePoint	= scale(f)
	def /!(f:Double):DoublePoint	= unscale(f)

	//------------------------------------------------------------------------------

	// additive inverse
	def negate:DoublePoint	= DoublePoint(-x, -y)
	// multiplicative inverse
	def inverse:DoublePoint	= DoublePoint(1/x, 1/y)
	// diagonal mirror
	def swap:DoublePoint	= DoublePoint(y,	x)

	def add(that:DoublePoint):DoublePoint	= DoublePoint(this.x + that.x, this.y + that.y)
	def sub(that:DoublePoint):DoublePoint	= DoublePoint(this.x - that.x, this.y - that.y)

	def mul(that:DoublePoint):DoublePoint	= DoublePoint(this.x * that.x, this.y * that.y)
	def div(that:DoublePoint):DoublePoint	= DoublePoint(this.x / that.x, this.y / that.y)

	def scale(f:Double):DoublePoint		= DoublePoint(x * f, y * f)
	def unscale(f:Double):DoublePoint	= DoublePoint(x / f, y / f)

	def signum:DoublePoint	= DoublePoint(smath signum x, smath signum y)

	//------------------------------------------------------------------------------

	def rectTo(that:DoublePoint):DoubleRect		= DoubleRect.topLeftToBottomRight	(this, that)
	def rectSize(that:DoublePoint):DoubleRect	= DoubleRect.topLeftWithSize 		(this, that)

	//------------------------------------------------------------------------------

	def angle:Double	= smath.atan2(y, x)

	def length:Double	= smath sqrt lengthQ
	def lengthQ:Double	= x*x + y*y

	def distance(that:DoublePoint):Double	= (that - this).length
	def distanceQ(that:DoublePoint):Double	= (that - this).lengthQ

	def rotate(angle:Double):DoublePoint	= {
		val	s	= smath sin angle
		val	c	= smath cos angle
		val xx	= x * c - y * s
		val yy	= x * s + y * c
		DoublePoint(xx, yy)
	}
}
