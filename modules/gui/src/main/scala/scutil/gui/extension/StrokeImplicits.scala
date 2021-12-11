package scutil.gui.extension

import java.awt.Stroke

import scutil.gui.CompositeStroke

object StrokeImplicits {
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
