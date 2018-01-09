package scutil.gui.pimp

import java.awt.{ List=>_, _ }

import scutil.geom._
import scutil.gui.geomConversion

object PointImplicits extends PointImplicits

trait PointImplicits {
	implicit final class PointExt(peer:Point) {
		@deprecated("use IntPoint", "0.134.0")
		def unary_- :Point	=
				new Point(
					-peer.x,
					-peer.y
				)
				
		@deprecated("use IntPoint", "0.134.0")
		def +(that:Point):Point	=
				new Point(
					peer.x + that.x,
					peer.y + that.y
				)
		
		@deprecated("use IntPoint", "0.134.0")
		def -(that:Point):Point	=
				new Point(
					peer.x	- that.x,
					peer.y	- that.y
				)
				
		@deprecated("use IntPoint", "0.134.0")
		def *(that:Point):Point	=
				new Point(
					peer.x  * that.x,
					peer.y	* that.y
				)
				
		@deprecated("use IntPoint", "0.134.0")
		def /(that:Point):Point	=
				new Point(
					peer.x / that.x,
					peer.y / that.y
				)
				
		@deprecated("use IntPoint", "0.134.0")
		def *!(factor:Int):Point	=
				new Point(
					peer.x  * factor,
					peer.y * factor
				)
				
		@deprecated("use IntPoint", "0.134.0")
		def /!(factor:Int):Point	=
				new Point(
					peer.x / factor,
					peer.y / factor
				)
	
		//------------------------------------------------------------------------------
		
		@deprecated("use IntPoint", "0.134.0")
		def moveBy(dimension:Dimension):Point	=
				new Point(
					peer.x + dimension.width,
					peer.y + dimension.height
				)
					
		@deprecated("use IntPoint", "0.134.0")
		def sizeTo(that:Point):Dimension	=
				new Dimension(
					that.x - peer.x,
					that.y - peer.y
				)
		
		@deprecated("use IntPoint", "0.134.0")
		def rectangleTo(that:Point):Rectangle	=
				new Rectangle(
					peer.x,
					peer.y,
					that.x - peer.x,
					that.y - peer.y
				)
		
		@deprecated("use IntPoint", "0.134.0")
		def rectangleWith(dimension:Dimension):Rectangle	=
				new Rectangle(
					peer,
					dimension
				)
				
		//------------------------------------------------------------------------------
				
		def toDimension:Dimension	=
				new Dimension(
					peer.x,
					peer.y
				)
				
		def toIntPoint:IntPoint	=
				geomConversion Point_IntPoint peer
	}
}
