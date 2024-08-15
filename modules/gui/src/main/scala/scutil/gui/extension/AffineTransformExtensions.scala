package scutil.gui.extension

import java.awt.geom.AffineTransform

import scutil.gui.*

object AffineTransformExtensions {
	extension (delegate:AffineTransform) {
		def toSafeAffineTransform:SafeAffineTransform	= SafeAffineTransform.fromAwtAffineTransform(delegate)
	}
}
