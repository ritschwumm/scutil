package scutil.color

import scala.math.*

import scutil.math.functions.*

object Alpha {
	val transparent	= Alpha(0)
	val opaque		= Alpha(1)
}

/** value range is 0..1 */
final case class Alpha(a:Float) {
	def diff(that:Alpha):Float	=
		abs(this.a - that.a)

	def inverse:Alpha	= Alpha(1-a)

	def blendAlpha(off:Alpha, on:Alpha):Alpha	=
		Alpha(lerpFloat(a, off.a, on.a))

	def blendRGB(off:RGB, on:RGB):RGB	=
		RGB(
			r	= lerpFloat(a, off.r, on.r),
			g	= lerpFloat(a, off.g, on.g),
			b	= lerpFloat(a, off.b, on.b)
		)

	def blendHSB(off:HSB, on:HSB):HSB	=
		HSB(
			h	= lerpFloat(a, off.h, on.h),
			s	= lerpFloat(a, off.s, on.s),
			b	= lerpFloat(a, off.b, on.b)
		)

	def blendRGBA(off:RGBA, on:RGBA):RGBA	=
		RGBA(
			blendRGB(off.rgb, on.rgb),
			blendAlpha(off.alpha, on.alpha)
		)

	def blendHSBA(off:HSBA, on:HSBA):HSBA	=
		HSBA(
			blendHSB(off.hsb, on.hsb),
			blendAlpha(off.alpha, on.alpha)
		)
}
