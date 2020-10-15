package scutil.gui

import java.awt.Graphics
import java.awt.Window
import javax.imageio.ImageReader
import javax.imageio.ImageWriter

import scutil.lang.tc._

object instances extends instances

trait instances {
	implicit def GraphicsResource[T<:Graphics]:Resource[T]			= Resource instance (_.dispose())
	implicit def ImageReaderResource[T<:ImageReader]:Resource[T]	= Resource instance (_.dispose())
	implicit def ImageWriterResource[T<:ImageWriter]:Resource[T]	= Resource instance (_.dispose())
	implicit def WindowResource[T<:Window]:Resource[T]				= Resource instance (_.dispose())
}
