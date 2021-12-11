package scutil.gui.extension

import java.awt.{ List=>_, _ }

import scutil.geom._
import scutil.gui.geomConversion

object RectangleImplicits {
	implicit final class RectangleExt(peer:Rectangle) {
		/*
		def inset(insets:Insets):Rectangle	=
			new Rectangle(
				peer.x + insets.left,
				peer.y + insets.top,
				peer.width	- insets.left	- insets.right,
				peer.height	- insets.top	- insets.bottom
			)
		*/

		def toIntRect:IntRect	=
			geomConversion Rectangle_IntRect peer
	}
}
