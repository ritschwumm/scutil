package scutil.geom

object DoubleRect {
	val zero	= new DoubleRect(DoubleSpan.zero, DoubleSpan.zero)

	def zeroSized(size:DoublePoint):DoubleRect	=
		new DoubleRect(
			horizontal	= DoubleSpan.startSize(0, size.x),
			vertical	= DoubleSpan.startSize(0, size.y)
		)

	def leftTopRightBottom(left:Double, top:Double, right:Double, bottom:Double):DoubleRect	=
		new DoubleRect(
			horizontal	= DoubleSpan.startEnd(left, right),
			vertical	= DoubleSpan.startEnd(top, bottom)
		)

	def leftTopWidthHeight(left:Double, top:Double, width:Double, height:Double):DoubleRect	=
		new DoubleRect(
			horizontal	= DoubleSpan.startSize(left, width),
			vertical	= DoubleSpan.startSize(top, height)
		)

	def topLeftToBottomRight(topLeft:DoublePoint, bottomRight:DoublePoint):DoubleRect	=
		new DoubleRect(
			horizontal	= DoubleSpan.startEnd(topLeft.x, bottomRight.x),
			vertical	= DoubleSpan.startEnd(topLeft.y, bottomRight.y)
		)

	def topLeftWithSize(topLeft:DoublePoint, size:DoublePoint):DoubleRect	=
		new DoubleRect(
			horizontal	= DoubleSpan.startSize(topLeft.x, size.x),
			vertical	= DoubleSpan.startSize(topLeft.y, size.y)
		)

	def horizontalWithVertical(horizontal:DoubleSpan, vertical:DoubleSpan):DoubleRect	=
		new DoubleRect(horizontal, vertical)
}

final class DoubleRect private (val horizontal:DoubleSpan, val vertical:DoubleSpan) {
	def left:Double		= horizontal.start
	def right:Double	= horizontal.end
	def top:Double		= vertical.start
	def bottom:Double	= vertical.end
	def width:Double	= horizontal.size
	def height:Double	= vertical.size

	def center:DoublePoint	= DoublePoint(horizontal.center, vertical.center)

	def size:DoublePoint	= DoublePoint(horizontal.size,	vertical.size)

	def topLeft:DoublePoint		= DoublePoint(left,		top)
	def topRight:DoublePoint	= DoublePoint(right,	top )
	def bottomLeft:DoublePoint	= DoublePoint(left,		bottom)
	def bottomRight:DoublePoint	= DoublePoint(right,	bottom)

	def contains(pt:DoublePoint):Boolean	=
		(horizontal	contains pt.x) &&
		(vertical	contains pt.y)

	def swap:DoubleRect			= new DoubleRect(vertical, horizontal)
	def negate:DoubleRect		= new DoubleRect(horizontal.negate,		vertical.negate)
	def normalize:DoubleRect	= new DoubleRect(horizontal.normalize,	vertical.normalize)

	def move(d:DoublePoint):DoubleRect		= new DoubleRect(horizontal move	d.x,	vertical move		d.y)
	def unmove(d:DoublePoint):DoubleRect	= new DoubleRect(horizontal unmove	d.x,	vertical unmove		d.y)

	def scale(f:DoublePoint):DoubleRect		= new DoubleRect(horizontal scale	f.x,	vertical scale		f.y)
	def unscale(f:DoublePoint):DoubleRect	= new DoubleRect(horizontal unscale	f.x,	vertical unscale	f.y)

	def union(that:DoubleRect):DoubleRect	=
		new DoubleRect(
			this.horizontal union that.horizontal,
			this.vertical	union that.vertical
		)

	def intersect(that:DoubleRect):Option[DoubleRect]	=
		(this.horizontal intersect that.horizontal, this.vertical intersect that.vertical) match {
			case (Some(horizontal), Some(vertical))	=> Some(new DoubleRect(horizontal, vertical))
			case _									=> None
		}

	def inset(left:Double, top:Double, right:Double,  bottom:Double):DoubleRect	=
		new DoubleRect(
			horizontal	.inset (left,	right),
			vertical	.inset (top,	bottom)
		)

	def transformer(that:DoubleRect):DoublePoint=>DoublePoint	=
		pos => (pos - this.topLeft) * that.size / this.size + that.topLeft

	//------------------------------------------------------------------------------

	override def equals(that:Any):Boolean	=
		that match {
			case that:DoubleRect	=> this.horizontal == that.horizontal && this.vertical == that.vertical
			case _					=> false
		}

	override def hashCode():Int		= this.horizontal.hashCode ^ this.vertical.hashCode

	override def toString:String	= s"DoubleRect(left=${left.toString}, top=${top.toString}, right=${right.toString}, bottom=${bottom.toString}, width=${width.toString}, height=${height.toString})"
}
