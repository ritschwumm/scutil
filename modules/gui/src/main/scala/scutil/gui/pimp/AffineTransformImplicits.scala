package scutil.gui.pimp

import java.awt.geom.AffineTransform

import scutil.gui._

object AffineTransformImplicits extends AffineTransformImplicits

trait AffineTransformImplicits {
	implicit final class AffineTransformExt(delegate:AffineTransform) {
		def toSafeAffineTransform:SafeAffineTransform	= SafeAffineTransform fromAwtAffineTransform delegate
	}
}
