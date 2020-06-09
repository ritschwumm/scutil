package scutil.geom

object IntRect {
	val zero	= new IntRect(IntSpan.zero, IntSpan.zero)

	def leftTopRightBottom(left:Int, top:Int, right:Int, bottom:Int):IntRect	=
		new IntRect(
			horizontal	= IntSpan startEnd (left, right),
			vertical	= IntSpan startEnd (top, bottom)
		)

	def leftTopWidthHeight(left:Int, top:Int, width:Int, height:Int):IntRect	=
		new IntRect(
			horizontal	= IntSpan startSize (left, width),
			vertical	= IntSpan startSize (top, height)
		)

	def topLeftToBottomRight(topLeft:IntPoint, bottomRight:IntPoint):IntRect	=
		new IntRect(
			horizontal	= IntSpan startEnd (topLeft.x, bottomRight.x),
			vertical	= IntSpan startEnd (topLeft.y, bottomRight.y)
		)

	def topLeftWithSize(topLeft:IntPoint, size:IntPoint):IntRect	=
		new IntRect(
			horizontal	= IntSpan startSize (topLeft.x, size.x),
			vertical	= IntSpan startSize (topLeft.y, size.y)
		)

	def horizontalWithVertical(horizontal:IntSpan, vertical:IntSpan):IntRect	=
		new IntRect(horizontal, vertical)
}

final class IntRect private (val horizontal:IntSpan, val vertical:IntSpan) {
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

	def swap:IntRect			= new IntRect(vertical, horizontal)
	def negate:IntRect			= new IntRect(horizontal.negate,	vertical.negate)
	def normalize:IntRect		= new IntRect(horizontal.normalize,	vertical.normalize)

	def move(d:IntPoint):IntRect	= new IntRect(horizontal move		d.x,	vertical move		d.y)
	def unmove(d:IntPoint):IntRect	= new IntRect(horizontal unmove		d.x,	vertical unmove		d.y)

	def scale(f:IntPoint):IntRect	= new IntRect(horizontal scale		f.x,	vertical scale		f.y)
	def unscale(f:IntPoint):IntRect	= new IntRect(horizontal unscale	f.x,	vertical unscale	f.y)

	def union(that:IntRect):IntRect	=
		new  IntRect(
			this.horizontal union that.horizontal,
			this.vertical	union that.vertical
		)

	def intersect(that:IntRect):Option[IntRect]	=
		(this.horizontal intersect that.horizontal, this.vertical intersect that.vertical) match {
			case (Some(horizontal), Some(vertical))	=> Some(new  IntRect(horizontal, vertical))
			case _									=> None
		}

	def toDoubleRect:DoubleRect	= DoubleRect horizontalWithVertical (horizontal.toDoubleSpan, vertical.toDoubleSpan)

	//------------------------------------------------------------------------------

	override def equals(that:Any):Boolean	=
		that match {
			case that:IntRect	=> this.horizontal == that.horizontal && this.vertical == that.vertical
			case _				=> false
		}

	override def hashCode():Int		= this.horizontal.hashCode ^ this.vertical.hashCode

	override def toString:String	= s"IntRect(left=${left.toString}, top=${top.toString}, right=${right.toString}, bottom=${bottom.toString}, width=${width.toString}, height=${height.toString})"
}
