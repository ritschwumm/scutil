package scutil.gui.pimp

import java.awt.{ List=>_, _ }

object InsetsImplicits extends InsetsImplicits

trait InsetsImplicits {
	implicit final class InsetsExt(peer:Insets) {
		@deprecated("use IntPoint", "0.134.0")
		def unary_! :Insets	=
				new Insets(
					-peer.top,
					-peer.left,
					-peer.bottom,
					-peer.right
				)
					
		@deprecated("use IntPoint", "0.134.0")
		def +(that:Insets):Insets	=
				new Insets(
					peer.top	+ that.top,
					peer.left	+ that.left,
					peer.bottom	+ that.bottom,
					peer.right	+ that.right
				)
				
		@deprecated("use IntPoint", "0.134.0")
		def -(that:Insets):Insets	=
				new Insets(
					peer.top	- that.top,
					peer.left	- that.left,
					peer.bottom	- that.bottom,
					peer.right	- that.right
				)
				
		@deprecated("use IntPoint", "0.134.0")
		def *(factor:Int):Insets	=
				new Insets(
					peer.top	* factor,
					peer.left	* factor,
					peer.bottom	* factor,
					peer.right	* factor
				)
				
		@deprecated("use IntPoint", "0.134.0")
		def /(factor:Int):Insets	=
				new Insets(
					peer.top	/ factor,
					peer.left	/ factor,
					peer.bottom	/ factor,
					peer.right	/ factor
				)
	}
}
