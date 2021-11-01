package scutil.gui.geom

import java.awt.geom.{ Line2D }

object SgLine {
	//------------------------------------------------------------------------------
	//## simple values

	val zero	= startEnd(SgPoint.zero, SgPoint.zero)
	val one		= startEnd(SgPoint.zero, SgPoint.one)

	//------------------------------------------------------------------------------
	//## component factory

	def horizontal(x:SgSpan, y:Double):SgLine	=
		startEnd(
			SgPoint(x.start, y),
			SgPoint(x.end, y)
		)

	def vertical(x:Double, y:SgSpan):SgLine	=
		startEnd(
			SgPoint(x, y.start),
			SgPoint(x, y.end)
		)

	def startEnd(start:SgPoint, end:SgPoint):SgLine	=
		new SgLine(start, end)

	def startBy(start:SgPoint, size:SgPoint):SgLine	=
		startEnd(start, start+size)

	def endBy(end:SgPoint, size:SgPoint):SgLine		=
		startEnd(end-size, end)

	//------------------------------------------------------------------------------
	//## awt conversion

	def fromAwtLine2D(it:Line2D):SgLine	=
		startEnd(
			SgPoint(
				it.getX1,
				it.getY1
			),
			SgPoint(
				it.getX2,
				it.getY2
			)
		)

	def toAwtLine(it:SgLine):Line2D	=
		it.toAwtLine2D
}

final case class SgLine private (start:SgPoint, end:SgPoint) {
	def x:SgSpan		= SgSpan.startEnd(start.x, end.x)
	def y:SgSpan		= SgSpan.startEnd(start.y, end.y)

	def size:SgPoint	= end - start
	def swap:SgLine		= SgLine.startEnd(end, start)

	def move(offset:SgPoint):SgLine =
		SgLine.startEnd(
			start	+ offset,
			end		+ offset
		)

	// TODO
	// def intersect(that:SgLine):Option[SgPoint]
	// def intersectInside(that:SgLine):Option[SgPoint]

	//------------------------------------------------------------------------------
	//## awt conversion

	def toAwtLine2D:Line2D	=
		new Line2D.Double(
			start.x,	start.y,
			end.x,		end.y
		)
}
