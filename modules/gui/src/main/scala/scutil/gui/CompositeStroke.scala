package scutil.gui

import java.awt.Shape
import java.awt.Stroke

final class CompositeStroke(a:Stroke, b:Stroke) extends Stroke {
	def createStrokedShape(shape:Shape):Shape	=
		a createStrokedShape (b createStrokedShape shape)
}
