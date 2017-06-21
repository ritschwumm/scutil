package scutil.geom

object DoubleRect {
	val zero	= DoubleRect(DoubleSpan.zero, DoubleSpan.zero)
	
	def leftTopRightBottom(left:Double, top:Double, right:Double, bottom:Double):DoubleRect	=
			DoubleRect(
				horizontal	= DoubleSpan startEnd (left, right),
				vertical	= DoubleSpan startEnd (top, bottom)
			)
			
	def leftTopWidthHeight(left:Double, top:Double, width:Double, height:Double):DoubleRect	=
			DoubleRect(
				horizontal	= DoubleSpan startSize (left, width),
				vertical	= DoubleSpan startSize (top, height)
			)
			
	def topLeftToBottomRight(topLeft:DoublePoint, bottomRight:DoublePoint):DoubleRect	=
			DoubleRect(
				horizontal	= DoubleSpan startEnd (topLeft.x, bottomRight.x),
				vertical	= DoubleSpan startEnd (topLeft.y, bottomRight.y)
			)
			
	def topLeftWithSize(topLeft:DoublePoint, size:DoublePoint):DoubleRect	=
			DoubleRect(
				horizontal	= DoubleSpan startSize (topLeft.x, size.x),
				vertical	= DoubleSpan startSize (topLeft.y, size.y)
			)
			
	def horizontalWithVertical(horizontal:DoubleSpan, vertical:DoubleSpan):DoubleRect	=
			DoubleRect(horizontal, vertical)
}

final case class DoubleRect(horizontal:DoubleSpan, vertical:DoubleSpan) {
	def left:Double		= horizontal.start
	def right:Double	= horizontal.end
	def top:Double		= vertical.start
	def bottom:Double	= vertical.end
	def width:Double	= horizontal.size
	def height:Double	= vertical.size
	
	def size:DoublePoint	= DoublePoint(horizontal.size,	vertical.size)
	
	def topLeft:DoublePoint		= DoublePoint(left, 	top)
	def topRight:DoublePoint	= DoublePoint(right,	top )
	def bottomLeft:DoublePoint	= DoublePoint(left,	bottom)
	def bottomRight:DoublePoint	= DoublePoint(right,	bottom)
	
	def contains(pt:DoublePoint):Boolean	=
			(horizontal	contains pt.x) &&
			(vertical	contains pt.y)
	
	def swap:DoubleRect			= DoubleRect(vertical, horizontal)
	def negate:DoubleRect		= DoubleRect(horizontal.negate,		vertical.negate)
	def normalize:DoubleRect	= DoubleRect(horizontal.normalize,	vertical.normalize)
	
	def move(d:DoublePoint):DoubleRect		= DoubleRect(horizontal move	d.x,	vertical move		d.y)
	def unmove(d:DoublePoint):DoubleRect	= DoubleRect(horizontal unmove	d.x,	vertical unmove		d.y)
	
	def scale(f:DoublePoint):DoubleRect		= DoubleRect(horizontal scale	f.x,	vertical scale		f.y)
	def unscale(f:DoublePoint):DoubleRect	= DoubleRect(horizontal unscale	f.x,	vertical unscale	f.y)
	
	def union(that:DoubleRect):DoubleRect	=
			DoubleRect(
				this.horizontal union that.horizontal,
				this.vertical	union that.vertical
			)
}
