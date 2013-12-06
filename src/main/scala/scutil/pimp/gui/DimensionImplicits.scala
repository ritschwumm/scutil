package scutil.pimp

import java.awt.{ List=>AwtList, _ }

import scutil.geom._

object DimensionImplicits extends DimensionImplicits

trait DimensionImplicits {
	implicit def toDimensionExt(peer:Dimension):DimensionExt	= new DimensionExt(peer)
}
	
final class DimensionExt(peer:Dimension) {
	def unary_- :Dimension	= new Dimension(
			-peer.width,
			-peer.height)
			
	def +(that:Dimension):Dimension	= new Dimension(
			peer.width  + that.width,
			peer.height + that.height)
	
	def -(that:Dimension):Dimension	= new Dimension(
			peer.width	- that.width,
			peer.height	- that.height)
			
	def *(that:Dimension):Dimension	= new Dimension(
			peer.width  * that.width,
			peer.height * that.height)
			
	def /(that:Dimension):Dimension	= new Dimension(
			peer.width  / that.width,
			peer.height / that.height)
			
	def *!(factor:Int):Dimension	= new Dimension(
			peer.width  * factor,
			peer.height * factor)
			
	def /!(factor:Int):Dimension	= new Dimension(
			peer.width  / factor,
			peer.height / factor)
	
	//------------------------------------------------------------------------------
			
	def toPoint:Point	= new Point(
			peer.width,
			peer.height)
			
	def toIntPoint:IntPoint	=
			IntPoint fromAwtDimension peer
}
