package scutil.gui.pimp

import java.awt.{ List=>AwtList, _ }
import javax.swing._

import scutil.geom._
import scutil.gui.geomConversion

object WindowImplicits extends WindowImplicits

trait WindowImplicits {
	implicit def toWindowExt(peer:Window):WindowExt	= new WindowExt(peer)
}
	
final class WindowExt(peer:Window) {
	def restrictToScreen() {
		val frame	= geomConversion Rectangle_IntRect peer.getBounds
		val screen	= geomConversion Rectangle_IntRect peer.getGraphicsConfiguration.getBounds
		peer setBounds (geomConversion IntRect_Rectangle (frame restrictTo screen))
	}
}
