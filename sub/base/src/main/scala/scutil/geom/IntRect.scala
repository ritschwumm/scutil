package scutil.geom

object IntRect {
	val zero	= IntRect(IntSpan.zero, IntSpan.zero)
	
	def leftTopRightBottom(left:Int, top:Int, right:Int, bottom:Int):IntRect	=
			IntRect(
				horizontal	= IntSpan startEnd (left, right),
				vertical	= IntSpan startEnd (top, bottom)
			)
			
	def leftTopWidthHeight(left:Int, top:Int, width:Int, height:Int):IntRect	=
			IntRect(
				horizontal	= IntSpan startSize (left, width),
				vertical	= IntSpan startSize (top, height)
			)
			
	def topLeftToBottomRight(topLeft:IntPoint, bottomRight:IntPoint):IntRect	=
			IntRect(
				horizontal	= IntSpan startEnd (topLeft.x, bottomRight.x),
				vertical	= IntSpan startEnd (topLeft.y, bottomRight.y)
			)
			
	def topLeftWithSize(topLeft:IntPoint, size:IntPoint):IntRect	=
			IntRect(
				horizontal	= IntSpan startSize (topLeft.x, size.x),
				vertical	= IntSpan startSize (topLeft.y, size.y)
			)
			
	def horizontalWithVertical(horizontal:IntSpan, vertical:IntSpan):IntRect	=
			IntRect(horizontal, vertical)
}

final case class IntRect(horizontal:IntSpan, vertical:IntSpan) {
	def left:Int	= horizontal.start
	def right:Int	= horizontal.end
	def top:Int		= vertical.start
	def bottom:Int	= vertical.end
	def width:Int	= horizontal.size
	def height:Int	= vertical.size
	
	def size:IntPoint	= IntPoint(horizontal.size,	vertical.size)
	
	def topLeft:IntPoint		= IntPoint(left, 	top)
	def topRight:IntPoint		= IntPoint(right,	top )
	def bottomLeft:IntPoint		= IntPoint(left,	bottom)
	def bottomRight:IntPoint	= IntPoint(right,	bottom)
	
	def contains(pt:IntPoint):Boolean	=
			(horizontal	contains pt.x) &&
			(vertical	contains pt.y)
	
	def swap:IntRect			= IntRect(vertical, horizontal)
	def negate:IntRect			= IntRect(horizontal.negate,	vertical.negate)
	def normalize:IntRect		= IntRect(horizontal.normalize,	vertical.normalize)
	
	def move(d:IntPoint):IntRect	= IntRect(horizontal move		d.x,	vertical move		d.y)
	def unmove(d:IntPoint):IntRect	= IntRect(horizontal unmove		d.x,	vertical unmove		d.y)
	
	def scale(f:IntPoint):IntRect	= IntRect(horizontal scale		f.x,	vertical scale		f.y)
	def unscale(f:IntPoint):IntRect	= IntRect(horizontal unscale	f.x,	vertical unscale	f.y)
	
	def union(that:IntRect):IntRect	=
			IntRect(
				this.horizontal union that.horizontal,
				this.vertical	union that.vertical
			)
			
	def intersect(that:IntRect):Option[IntRect]	=
			(this.horizontal intersect that.horizontal, this.vertical intersect that.vertical) match {
				case (Some(horizontal), Some(vertical))	=> Some(IntRect(horizontal, vertical))
				case _									=> None
			}
			
	def toDoubleRect:DoubleRect	= DoubleRect(horizontal.toDoubleSpan, vertical.toDoubleSpan)
}
