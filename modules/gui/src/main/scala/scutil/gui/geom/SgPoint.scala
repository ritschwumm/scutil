package scutil.gui.geom

import java.awt.geom.{ Point2D, Dimension2D }

import scala.math.*

import scutil.gui.Dimension2D_Double

object SgPoint {
	//------------------------------------------------------------------------------
	//## simple values

	val zero	= SgPoint(0, 0)
	val one		= SgPoint(1, 1)

	//------------------------------------------------------------------------------
	//## compoment factory

	def symmetric(value:Double)	= SgPoint(value, value)

	//------------------------------------------------------------------------------
	//## orientation factory

	def orientationWith(orientation:SgOrientation, master:Double, slave:Double):SgPoint	=
		orientation match {
			case SgOrientation.Horizontal	=> SgPoint(master,	slave)
			case SgOrientation.Vertical		=> SgPoint(slave,	master)
		}

	//------------------------------------------------------------------------------
	//## pair conversion

	def fromPair(it:(Double,Double)):SgPoint	=
		SgPoint(it._1, it._2)

	def toPair(it:SgPoint):(Double,Double)	=
		it.toPair

	//------------------------------------------------------------------------------
	//## awt conversion

	def fromAwtPoint2D(it:Point2D):SgPoint	=
		SgPoint(it.getX, it.getY)

	def toAwtPoint2D(it:SgPoint):Point2D	=
		it.toAwtPoint2D

	def fromAwtDimension2D(it:Dimension2D):SgPoint	=
		SgPoint(it.getWidth, it.getHeight)

	def toAwtDimension2D(it:SgPoint):Dimension2D	=
		it.toAwtDimension2D
}

final case class SgPoint(x:Double, y:Double) {
	def unary_- :SgPoint	= SgPoint(-x, -y)
	def swap:SgPoint		= SgPoint(y,x)

	def addInverse:SgPoint	= SgPoint(-x, -y)
	def mulInverse:SgPoint	= SgPoint(1/x, 1/y)

	def +(that:SgPoint):SgPoint	= SgPoint(this.x+that.x, this.y+that.y)
	def -(that:SgPoint):SgPoint	= SgPoint(this.x-that.x, this.y-that.y)

	def *(value:Double):SgPoint	= SgPoint(x*value, y*value)
	def /(value:Double):SgPoint	= SgPoint(x/value, y/value)

	def scale(that:SgPoint):SgPoint		= SgPoint(this.x*that.x, this.y*that.y)
	def descale(that:SgPoint):SgPoint	= SgPoint(this.x/that.x, this.y/that.y)

	def signum:SgPoint	= SgPoint(scala.math.signum(this.x), scala.math.signum(this.y))

	def length:Double	= sqrt(lengthQ)
	def lengthQ:Double	= x*x + y*y

	def distance(that:SgPoint):Double	= (that - this).length
	def distanceQ(that:SgPoint):Double	= (that - this).lengthQ

	def angle:Double	= atan2(y, x)

	/*
	def normalize:SgPoint	= {
		val	len	= length
		if (abs(len) < 0.0001)	SgPoint.zero
		else					this / len
	}
	*/

	def rotate(angle:Double):SgPoint	= {
		val	s	= sin(angle)
		val	c	= cos(angle)
		val xx	= x * c - y * s
		val yy	= x * s + y * c
		SgPoint(xx, yy)
	}

	/*
	def shearX(value:Double)	= Kartesian(x + y * value, y)
	def shearY(value:Double)	= Kartesian(x, y + x * value)

	mirrorX:	function()			new Point(-this.x,  this.y),
	mirrorY:	function()			new Point( this.x, -this.y),

	// TODO extend
	// dot product
	dot:		function(that)		this.x * that.x + this.x * that.y + this.y * that.x + this.y * that.y,
	// cross product analog 1: magnitude (= z-coordinate, x and y will be 0) for the cross-product of two zero-z 3d vectors
	crossZ:		function(that)		this.x * that.y - this.y * that.x,
	// cross product analog 2: perpendicular vector
	crossG:		function()			Point(this.y, -this.x),
	*/

	//------------------------------------------------------------------------------
	//## factory dsl

	def lineTo(that:SgPoint):SgLine				= SgLine		.startEnd	(this, that)
	def lineBy(size:SgPoint):SgLine				= SgLine		.startBy	(this, size)
	def rectangleTo(that:SgPoint):SgRectangle	= SgRectangle	.topLeftTo	(this, that)
	def rectangleBy(size:SgPoint):SgRectangle	= SgRectangle	.topLeftBy	(this, size)

	//------------------------------------------------------------------------------
	//## orientation lens

	def get(orientation:SgOrientation):Double	=
		orientation match {
			case SgOrientation.Horizontal	=> x
			case SgOrientation.Vertical		=> y
		}

	def set(orientation:SgOrientation, it:Double):SgPoint	=
		orientation match {
			case SgOrientation.Horizontal	=> SgPoint(it, y)
			case SgOrientation.Vertical		=> SgPoint(x, it)
		}

	def modify(orientation:SgOrientation, it:Double=>Double):SgPoint	=
		orientation match {
			case SgOrientation.Horizontal	=> SgPoint(it(x), y)
			case SgOrientation.Vertical		=> SgPoint(x, it(y))
		}

	//------------------------------------------------------------------------------
	//## internal conversion

	def toPolar	= SgPolar(length, angle)

	//------------------------------------------------------------------------------
	//## pair conversion

	def toPair:(Double,Double)	= (x, y)

	//------------------------------------------------------------------------------
	//## awt conversion

	def toAwtPoint2D:Point2D			= new Point2D.Double(x,y)
	def toAwtDimension2D:Dimension2D	= new Dimension2D_Double(x,y)
}
