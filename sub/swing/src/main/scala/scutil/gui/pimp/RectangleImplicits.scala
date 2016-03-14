package scutil.gui.pimp

import java.awt.{ List=>_, _ }

import scutil.geom._
import scutil.gui.geomConversion

object RectangleImplicits extends RectangleImplicits

trait RectangleImplicits {
	implicit def toRectangleExt(peer:Rectangle):RectangleExt	= new RectangleExt(peer)
}
	
final class RectangleExt(peer:Rectangle) {
	def topLeft:Point		= new Point(
			peer.x,
			peer.y)
	
	def topRight:Point		= new Point(
			peer.x + peer.width,
			peer.y)
			
	def bottomLeft:Point	= new Point(
			peer.x,
			peer.y + peer.height)
			
	def bottomRight:Point	= new Point(
			peer.x + peer.width,
			peer.y + peer.height)
			
	//------------------------------------------------------------------------------
			
	def inset(insets:Insets):Rectangle	= new Rectangle(
			peer.x + insets.left,
			peer.y + insets.top,
			peer.width	- insets.left	- insets.right,
			peer.height	- insets.top	- insets.bottom)
			
	def toIntRect:IntRect	=
			geomConversion Rectangle_IntRect peer
}
