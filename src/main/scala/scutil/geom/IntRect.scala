package scutil.geom

import java.awt.{ Rectangle => AwtRectangle }

object IntRect {
	val zero	= IntRect(IntSpan.zero, IntSpan.zero)
	
	def fromAwtRectangle(awt:AwtRectangle):IntRect	=
			IntRect(IntSpan(awt.x, awt.width), IntSpan(awt.y, awt.height))
		
	def toAwtRectangle(own:IntRect):AwtRectangle	=
			own.toAwtRectangle
}

case class IntRect(x:IntSpan, y:IntSpan) {
	def withX(x:IntSpan):IntRect	= IntRect(x, y)
	def withY(y:IntSpan):IntRect	= IntRect(x, y)
	
	def left:Int	= x.start
	def right:Int	= x.end
	def top:Int		= y.start
	def bottom:Int	= y.end
	
	def width:Int	= x.size
	def height:Int	= x.size
	
	def topLeft:IntPoint		= IntPoint(x.start,	y.start)
	def topRight:IntPoint		= IntPoint(x.end,	y.start)
	def bottomLeft:IntPoint		= IntPoint(x.start,	y.end)
	def bottomRight:IntPoint	= IntPoint(x.end,	y.end)
	
	def size:IntPoint	= IntPoint(x.size,	y.size)
	
	def unary_- :IntRect			= negate
	def + (that:IntPoint):IntRect	= move(that)
	def - (that:IntPoint):IntRect	= unmove(that)
	
	def swap:IntRect					= IntRect(y,					x)
	def negate:IntRect					= IntRect(-x,					-y)
	def move(offset:IntPoint):IntRect	= IntRect(x move	offset.x,	y move		offset.y)
	def unmove(offset:IntPoint):IntRect	= IntRect(x unmove	offset.x,	y unmove	offset.y)
	def normalize:IntRect				= IntRect(x.normalize,			y.normalize)

	def restrictTo(that:IntRect):IntRect	= 
			IntRect(
				this.x restrictTo that.x,
				this.y restrictTo that.y
			)
			
	def toAwtRectangle:AwtRectangle	= 
			new AwtRectangle(left, top, width, height)
}
