package scutil.gui.pimp

import java.awt.{ List=>_, _ }

import scutil.geom._
import scutil.gui.geomConversion

object RectangleImplicits extends RectangleImplicits

trait RectangleImplicits {
	implicit final class RectangleExt(peer:Rectangle) {
		@deprecated("use toIntRect.topLeft.toAwtPoint", "0.141.0")
		def topLeft:Point	=
				new Point(
					peer.x,
					peer.y
				)
		
		@deprecated("use toIntRect.topRight.toAwtPoint", "0.141.0")
		def topRight:Point	=
				new Point(
					peer.x + peer.width,
					peer.y
				)
				
		@deprecated("use toIntRect.bottomLeft.toAwtPoint", "0.141.0")
		def bottomLeft:Point	=
				new Point(
					peer.x,
					peer.y + peer.height
				)
				
		@deprecated("use toIntRect.bottomRight.toAwtPoint", "0.141.0")
		def bottomRight:Point	=
				new Point(
					peer.x + peer.width,
					peer.y + peer.height
				)
				
		//------------------------------------------------------------------------------
			
		// TODO get rid of this
		def inset(insets:Insets):Rectangle	=
				new Rectangle(
					peer.x + insets.left,
					peer.y + insets.top,
					peer.width	- insets.left	- insets.right,
					peer.height	- insets.top	- insets.bottom
				)
				
		def toIntRect:IntRect	=
				geomConversion Rectangle_IntRect peer
	}
}
