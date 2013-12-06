package scutil.pimp

import java.awt.{ List=>AwtList, _ }

import scutil.geom._

object PointImplicits extends PointImplicits

trait PointImplicits {
	implicit def toPointExt(peer:Point):PointExt	= new PointExt(peer)
}
	
final class PointExt(peer:Point) {
	def unary_- :Point	= new Point(
			-peer.x,
			-peer.y)
			
	def +(that:Point):Point	= new Point(
			peer.x + that.x,
			peer.y + that.y)
	
	def -(that:Point):Point	= new Point(
			peer.x	- that.x,
			peer.y	- that.y)
			
	def *(that:Point):Point	= new Point(
			peer.x  * that.x,
			peer.y	* that.y)
			
	def /(that:Point):Point	= new Point(
			peer.x / that.x,
			peer.y / that.y)
			
	def *!(factor:Int):Point	= new Point(
			peer.x  * factor,
			peer.y * factor)
			
	def /!(factor:Int):Point	= new Point(
			peer.x / factor,
			peer.y / factor)

	//------------------------------------------------------------------------------
			
	def moveBy(dimension:Dimension):Point	= new Point(
			peer.x + dimension.width,
			peer.y + dimension.height)
			
	def sizeTo(that:Point):Dimension		= new Dimension(
			that.x - peer.x,
			that.y - peer.y)
	
	def rectangleTo(that:Point):Rectangle	= new Rectangle(
			peer.x,
			peer.y,
			that.x - peer.x,
			that.y - peer.y)
	
	def rectangleWith(dimension:Dimension):Rectangle	= 
			new Rectangle(peer, dimension)
			
	//------------------------------------------------------------------------------
			
	def toDimension:Dimension	= new Dimension(
			peer.x,
			peer.y)
			
	def toIntPoint:IntPoint	=
			IntPoint fromAwtPoint peer
}
