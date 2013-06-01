package scutil.geom

import java.awt.{ Point => AwtPoint, Dimension => AwtDimension }

object IntPoint {
	val zero	= IntPoint(0, 0)
	val one		= IntPoint(1, 1)
	
	def fromAwtPoint(awt:AwtPoint):IntPoint	=
			IntPoint(awt.x, awt.y)
		
	def fromAwtDimension(awt:AwtDimension):IntPoint	=
			IntPoint(awt.width, awt.height)
		
	def toAwtPoint(own:IntPoint):AwtPoint	=
			own.toAwtPoint
		
	def toAwtDimension(own:IntPoint):AwtDimension	=
			own.toAwtDimension
}

case class IntPoint(x:Int, y:Int) {
	def withX(x:Int):IntPoint	= IntPoint(x, y)
	def withY(y:Int):IntPoint	= IntPoint(x, y)
	
	def unary_- :IntPoint			= negate
	def + (that:IntPoint):IntPoint	= move(that)
	def - (that:IntPoint):IntPoint	= unmove(that)
	def * (that:IntPoint):IntPoint	= scale(that)
	def / (that:IntPoint):IntPoint	= unscale(that)
	
	def swap:IntPoint					= IntPoint(y,				x)
	def negate:IntPoint					= IntPoint(-x,				-y)
	def move(that:IntPoint):IntPoint	= IntPoint(this.x + that.x,	this.y + that.y)
	def unmove(that:IntPoint):IntPoint	= IntPoint(this.x - that.x,	this.y - that.y)
	def scale(that:IntPoint):IntPoint	= IntPoint(this.x * that.x,	this.y * that.y)
	def unscale(that:IntPoint):IntPoint	= IntPoint(this.x / that.x,	this.y / that.y)
	
	def rectTo(that:IntPoint):IntRect	= 
			IntRect(
				IntSpan(this.x, that.x - this.x), 
				IntSpan(this.y, that.y - this.y)
			)
			
	def rectWith(that:IntPoint):IntRect	= 
			IntRect(
				IntSpan(this.x, that.x), 
				IntSpan(this.y, that.y)
			)
			
	def toAwtPoint:AwtPoint	=
			new AwtPoint(x, y)
		
	def toAwtDimension:AwtDimension	=
			new AwtDimension(x, y)
}
