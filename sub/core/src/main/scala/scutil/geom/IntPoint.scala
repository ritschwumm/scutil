package scutil.geom

object IntPoint {
	val zero	= IntPoint(0, 0)
	val one		= IntPoint(1, 1)
}

final case class IntPoint(x:Int, y:Int) {
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
}
