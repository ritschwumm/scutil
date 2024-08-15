package scutil.gui.extension

import java.awt.Stroke

import scutil.gui.CompositeStroke

object StrokeExtensions {
	extension (peer:Stroke) {
		/** symbolic alias for andThen */
		def >=>(that:Stroke):Stroke	=
			andThen(that)

		/** symbolic alias for compose */
		def <=<(that:Stroke):Stroke	=
			compose(that)

		def compose(that:Stroke):Stroke	= new CompositeStroke(peer, that)
		def andThen(that:Stroke):Stroke	= new CompositeStroke(that, peer)
	}
}
