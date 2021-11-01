package scutil.gui.geom

object SgLinearTransform1D {
	val identity	= factorSummand(1.0, 0.0)

	//------------------------------------------------------------------------------
	//## component factory

	def factorSummand(factor:Double, summand:Double):SgLinearTransform1D	= new SgLinearTransform1D(factor, summand)

	def fromTo(from:SgSpan, to:SgSpan):SgLinearTransform1D	= {
		val factor	= to.size / from.size
		val summand	= to.start - from.start * factor
		factorSummand(factor, summand)
	}
}

final case class SgLinearTransform1D private (factor:Double, summand:Double) {
	def inverse:SgLinearTransform1D	=
		SgLinearTransform1D.factorSummand(
			1			/ factor,
			-summand	/ factor
		)

	//------------------------------------------------------------------------------

	def apply(value:Double):Double		= transform(value)

	def transform(value:Double):Double	= value * factor + summand
	def scale(value:Double):Double		= value * factor
	def offset(value:Double):Double		= value + summand

	def transformSpan(value:SgSpan):SgSpan	=
		SgSpan.startEnd(
			transform(value.start),
			transform(value.end)
		)

	//------------------------------------------------------------------------------
	//## factory dsl

	def transform2DWith(y:SgLinearTransform1D):SgLinearTransform2D	=
		SgLinearTransform2D.xy(this, y)
}
