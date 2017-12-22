package scutil.gui.pimp

import java.awt.Stroke

import scutil.gui.CompositeStroke

object StrokeImplicits extends StrokeImplicits

trait StrokeImplicits {
	implicit final class StrokeExt(peer:Stroke) {
		/** symbolic alias for andThen */
		def >=>(that:Stroke):Stroke	=
				this andThen that
			
		/** symbolic alias for compose */
		def <=<(that:Stroke):Stroke	=
				this compose that
			
		def compose(that:Stroke):Stroke	= new CompositeStroke(peer, that)
		def andThen(that:Stroke):Stroke	= new CompositeStroke(that, peer)
	}
}
