package scutil.gui.geom

import java.awt.geom.{ Rectangle2D }

object SgRectangle {
	//------------------------------------------------------------------------------
	//## simple values

	val zero	= xy(SgSpan.zero,	SgSpan.zero)
	val one		= xy(SgSpan.one,	SgSpan.one)

	//------------------------------------------------------------------------------
	//## component factory

	def xy(x:SgSpan, y:SgSpan):SgRectangle	= new SgRectangle(x, y)

	def topLeftZeroBy(size:SgPoint):SgRectangle	=
		xy(
			SgSpan startZeroBy size.x,
			SgSpan startZeroBy size.y
		)

	def topLeftBy(pos:SgPoint, size:SgPoint):SgRectangle	=
		xy(
			SgSpan.startEnd(pos.x, pos.x+size.x),
			SgSpan.startEnd(pos.y, pos.y+size.y)
		)

	def topLeftTo(pos:SgPoint, other:SgPoint):SgRectangle	=
		xy(
			SgSpan.startEnd(pos.x, other.x),
			SgSpan.startEnd(pos.y, other.y)
		)

	def centerBy(center:SgPoint, size:SgPoint):SgRectangle	=
		xy(
			SgSpan.startEnd(center.x-size.x/2, center.x+size.x/2),
			SgSpan.startEnd(center.y-size.y/2, center.y+size.y/2)
		)

	//------------------------------------------------------------------------------
	//## orientation factory

	def orientationWith(orientation:SgOrientation, master:SgSpan, slave:SgSpan):SgRectangle	=
		orientation match {
			case SgOrientation.Horizontal	=> xy(master,	slave)
			case SgOrientation.Vertical		=> xy(slave,	master)
		}

	//------------------------------------------------------------------------------
	//## awt conversion

	def fromAwtRectangle2D(it:Rectangle2D):SgRectangle	=
		xy(
			SgSpan.startEnd(it.getX, it.getX+it.getWidth),
			SgSpan.startEnd(it.getY, it.getY+it.getHeight)
		)

	def toAwtRectangle2D(it:SgRectangle):Rectangle2D	=
		it.toAwtRectangle2D
}

final case class SgRectangle private (x:SgSpan, y:SgSpan) {
	def top:Double		= y.start
	def left:Double		= x.start
	def bottom:Double	= y.end
	def right:Double	= x.end

	def topLeft:SgPoint			= SgPoint(x.start,	y.start)
	def topRight:SgPoint		= SgPoint(x.end,	y.start)
	def bottomLeft:SgPoint		= SgPoint(x.start,	y.end)
	def bottomRight:SgPoint		= SgPoint(x.end,	y.end)

	def center:SgPoint			= SgPoint(x.center, y.center)
	def topCenter:SgPoint		= SgPoint(x.center, y.start)
	def bottomCenter:SgPoint	= SgPoint(x.center, y.end)
	def leftCenter:SgPoint		= SgPoint(x.start,	y.center)
	def rightCenter:SgPoint		= SgPoint(x.end,	y.center)

	def topLine:SgLine		= SgLine .horizontal	(x,		top)
	def bottomLine:SgLine	= SgLine .horizontal	(x,		bottom)
	def leftLine:SgLine		= SgLine .vertical		(left,	y)
	def rightLine:SgLine	= SgLine .vertical		(right,	y)

	def diagonal1:SgLine	= SgLine .startEnd (topLeft,		bottomRight)
	def diagonal2:SgLine	= SgLine .startEnd (topRight,	bottomLeft)

	def empty:Boolean		= x.empty || y.empty
	def normal:Boolean		= x.normal && y.normal
	def size:SgPoint		= SgPoint(x.size, y.size)

	def swap:SgRectangle	= SgRectangle.xy(x,y)

	def normalize:SgRectangle	=
		SgRectangle.xy(
			x.normalize,
			y.normalize
		)

	def union(that:SgRectangle):SgRectangle	=
		SgRectangle.xy(
			this.x union that.x,
			this.y union that.y
		)

	def intersect(that:SgRectangle):Option[SgRectangle]	=
		(this.x intersect that.x, this.y intersect that.y) match {
			case (Some(x), Some(y))	=> Some(SgRectangle.xy(x,y))
			case _					=> None
		}

	def intersects(that:SgRectangle):Boolean	=
		(this.x intersects that.x) &&
		(this.y intersects that.y)

	def containsPoint(point:SgPoint) =
		(x containsValue point.x)	&&
		(y containsValue point.y)

	def contains(that:SgRectangle):Boolean	=
		(this.x contains that.x) &&
		(this.y contains that.y)

	def move(offset:SgPoint):SgRectangle	=
		SgRectangle.xy(
			x move offset.x,
			y move offset.y
		)

	def inset(insets:SgRectangleInsets):SgRectangle	=
		SgRectangle.xy(
			x inset insets.x,
			y inset insets.y
		)

	def splitAtX(position:Double):(SgRectangle, SgRectangle)	= {
		val (a, b)	= x splitAt position
		(
			SgRectangle.xy(a, y),
			SgRectangle.xy(b, y)
		)
	}

	def splitAtY(position:Double):(SgRectangle, SgRectangle)	= {
		val (a, b)	= y splitAt position
		(
			SgRectangle.xy(x, a),
			SgRectangle.xy(x, b)
		)
	}

	def splitAtOrientation(position:Double, orientation:SgOrientation):(SgRectangle, SgRectangle)	=
		orientation.cata(
			splitAtX(position),
			splitAtY(position)
		)

	def splitAt(position:SgPoint):(SgRectangle, SgRectangle, SgRectangle, SgRectangle)	= {
		val (ax, bx)	= x splitAt position.x
		val (ay, by)	= y splitAt position.y
		(
			SgRectangle.xy(ax, ay),
			SgRectangle.xy(bx, ay),
			SgRectangle.xy(ax, by),
			SgRectangle.xy(bx, by)
		)
	}

	//------------------------------------------------------------------------------
	//## factory dsl

	def linearTransformTo(that:SgRectangle):SgLinearTransform2D	=
		SgLinearTransform2D.fromTo(this, that)

	def affineTransformTo(that:SgRectangle):SgAffineTransform	=
		linearTransformTo(that).toAffineTransform

	//------------------------------------------------------------------------------
	//## orientation lens

	def get(orientation:SgOrientation):SgSpan	=
		orientation match {
			case SgOrientation.Horizontal	=> x
			case SgOrientation.Vertical		=> y
		}

	def set(orientation:SgOrientation, it:SgSpan):SgRectangle	=
		orientation match {
			case SgOrientation.Horizontal	=> SgRectangle.xy(it, y)
			case SgOrientation.Vertical		=> SgRectangle.xy(x, it)
		}

	def modify(orientation:SgOrientation, it:SgSpan=>SgSpan):SgRectangle	=
		orientation match {
			case SgOrientation.Horizontal	=> SgRectangle.xy(it(x), y)
			case SgOrientation.Vertical		=> SgRectangle.xy(x, it(y))
		}

	//------------------------------------------------------------------------------
	//## awt conversion

	def toAwtRectangle2D:Rectangle2D	=
		new Rectangle2D.Double(
			x.start, y.start,
			x.size, y.size
		)
}
