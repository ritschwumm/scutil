package scutil.gui.pimp

import java.awt._

import scutil.geom._
import scutil.gui.geomConversion

object IntPointImplicits extends IntPointImplicits

trait IntPointImplicits {
	implicit final class IntPointExt(peer:IntPoint) {
		def toAwtPoint:Point	=
				geomConversion	IntPoint_Point		peer
			
		def toAwtDimension:Dimension	=
				geomConversion	IntPoint_Dimension	peer
	}
}
