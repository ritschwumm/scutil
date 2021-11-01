package scutil.gui.geom

object SgLinearTransform2D {
	//------------------------------------------------------------------------------
	//## simple values

	val identity	= factorSummand(SgPoint.one, SgPoint.zero)

	//------------------------------------------------------------------------------
	//##compoment factory

	def factorSummand(factor:SgPoint, summand:SgPoint):SgLinearTransform2D	=
		new SgLinearTransform2D(factor, summand)

	//------------------------------------------------------------------------------

	def fromTo(from:SgRectangle, to:SgRectangle):SgLinearTransform2D	=
		xy(
			SgLinearTransform1D.fromTo(from.x, to.x),
			SgLinearTransform1D.fromTo(from.y, to.y)
		)

	def xy(x:SgLinearTransform1D, y:SgLinearTransform1D):SgLinearTransform2D	=
		factorSummand(
			SgPoint(x.factor,	y.factor),
			SgPoint(x.summand,	y.summand)
		)
}

final case class SgLinearTransform2D private (factor:SgPoint, summand:SgPoint) {
	def x:SgLinearTransform1D	= SgLinearTransform1D.factorSummand(factor.x, summand.x)
	def y:SgLinearTransform1D	= SgLinearTransform1D.factorSummand(factor.y, summand.y)

	def inverse:SgLinearTransform2D	=
		SgLinearTransform2D.factorSummand(
			factor.mulInverse,
			-(summand descale factor)
		)

	//------------------------------------------------------------------------------

	def apply(value:SgPoint):SgPoint		= transform(value)

	def transform(value:SgPoint):SgPoint	= (value scale factor) + summand
	def scale(value:SgPoint):SgPoint		= value scale factor
	def offset(value:SgPoint):SgPoint		= value + summand

	def transformLine(value:SgLine):SgLine	=
		SgLine.startEnd(
			transform(value.start),
			transform(value.end)
		)

	def transformRectangle(value:SgRectangle):SgRectangle	=
		SgRectangle.xy(
			x transformSpan value.x,
			y transformSpan value.y
		)

	//------------------------------------------------------------------------------
	//## internal conversion

	def toAffineTransform:SgAffineTransform	=
		SgAffineTransform.identity translate summand scale factor
}
