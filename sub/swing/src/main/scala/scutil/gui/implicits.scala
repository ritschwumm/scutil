package scutil.gui

import scutil.gui.pimp._

object implicits extends implicits

trait implicits
		extends	disposables
		with	RGBImplicits
		with	RGBAImplicits
		with	HSBImplicits
		with	HSBAImplicits
		with	IntPointImplicits
		with	IntRectImplicits
		with	ColorImplicits
		with	ComponentImplicits
		with	ContainerImplicits
		with	DimensionImplicits
		with	GraphicsImplicits
		with	ImageImplicits
		with	ImageIconImplicits
		with	InsetsImplicits
		with	JComponentImplicits
		with	PointImplicits
		with	RectangleImplicits
		with	RootPaneContainerImplicits
		with	StrokeImplicits
		with	WindowImplicits
