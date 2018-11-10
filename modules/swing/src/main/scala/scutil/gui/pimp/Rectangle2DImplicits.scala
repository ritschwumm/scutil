package scutil.gui.pimp

import java.awt.geom._

import scutil.geom._
import scutil.gui.geomConversion

object Rectangle2DImplicits extends Rectangle2DImplicits

trait Rectangle2DImplicits {
	implicit final class Rectangle2DExt(peer:Rectangle2D) {
		/*
		def inset(insets:Insets):Rectangle2D	=
				new Rectangle2D(
					peer.x + insets.left,
					peer.y + insets.top,
					peer.width	- insets.left	- insets.right,
					peer.height	- insets.top	- insets.bottom
				)
		*/

		def toDoubleRect:DoubleRect	=
				geomConversion Rectangle2D_DoubleRect peer
	}
}
