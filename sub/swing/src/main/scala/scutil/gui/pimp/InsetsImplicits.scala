package scutil.gui.pimp

import java.awt.{ List=>AwtList, _ }

object InsetsImplicits extends InsetsImplicits

trait InsetsImplicits {
	implicit def toInsetsExt(peer:Insets):InsetsExt	= new InsetsExt(peer)
}
	
final class InsetsExt(peer:Insets) {
	def unary_! :Insets			= new Insets(
			-peer.top,
			-peer.left,
			-peer.bottom,
			-peer.right)
			
	def +(that:Insets):Insets	= new Insets(
			peer.top	+ that.top,
			peer.left	+ that.left,
			peer.bottom	+ that.bottom,
			peer.right	+ that.right)
			
	def -(that:Insets):Insets	= new Insets(
			peer.top	- that.top,
			peer.left	- that.left,
			peer.bottom	- that.bottom,
			peer.right	- that.right)
			
	def *(factor:Int):Insets	= new Insets(
			peer.top	* factor,
			peer.left	* factor,
			peer.bottom	* factor,
			peer.right	* factor)
			
	def /(factor:Int):Insets	= new Insets(
			peer.top	/ factor,
			peer.left	/ factor,
			peer.bottom	/ factor,
			peer.right	/ factor)
}
