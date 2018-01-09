package scutil.gui.pimp

import java.awt.{ List=>_, _ }

import scutil.geom._
import scutil.gui.geomConversion

object DimensionImplicits extends DimensionImplicits

trait DimensionImplicits {
	implicit final class DimensionExt(peer:Dimension) {
		@deprecated("use IntPoint", "0.134.0")
		def unary_- :Dimension	=
				new Dimension(
					-peer.width,
					-peer.height
				)
				
		@deprecated("use IntPoint", "0.134.0")
		def +(that:Dimension):Dimension	=
				new Dimension(
					peer.width  + that.width,
					peer.height + that.height
				)
		
		@deprecated("use IntPoint", "0.134.0")
		def -(that:Dimension):Dimension	=
				new Dimension(
					peer.width	- that.width,
					peer.height	- that.height
				)
				
		@deprecated("use IntPoint", "0.134.0")
		def *(that:Dimension):Dimension	=
				new Dimension(
					peer.width  * that.width,
					peer.height * that.height
				)
			
		@deprecated("use IntPoint", "0.134.0")
		def /(that:Dimension):Dimension	=
				new Dimension(
					peer.width  / that.width,
					peer.height / that.height
				)
				
		@deprecated("use IntPoint", "0.134.0")
		def *!(factor:Int):Dimension	=
				new Dimension(
					peer.width  * factor,
					peer.height * factor
				)
				
		@deprecated("use IntPoint", "0.134.0")
		def /!(factor:Int):Dimension	=
				new Dimension(
					peer.width  / factor,
					peer.height / factor
				)
		
		//------------------------------------------------------------------------------
				
		def toPoint:Point	=
				new Point(
					peer.width,
					peer.height
				)
				
		def toIntPoint:IntPoint	=
				geomConversion Dimension_IntPoint peer
	}
}
