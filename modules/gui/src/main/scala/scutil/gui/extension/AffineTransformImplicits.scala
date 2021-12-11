package scutil.gui.extension

import java.awt.geom.AffineTransform

import scutil.gui._

object AffineTransformImplicits {
	implicit final class AffineTransformExt(delegate:AffineTransform) {
		def toSafeAffineTransform:SafeAffineTransform	= SafeAffineTransform fromAwtAffineTransform delegate
	}
}
