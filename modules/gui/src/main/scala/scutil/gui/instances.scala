package scutil.gui

import java.awt.Graphics
import java.awt.Window
import javax.imageio.ImageReader
import javax.imageio.ImageWriter

import scutil.lang.Resource

object instances extends instances

trait instances {
	implicit def GraphicsResource[T<:Graphics]:Resource[T]			= _.dispose()
	implicit def ImageReaderResource[T<:ImageReader]:Resource[T]	= _.dispose()
	implicit def ImageWriterResource[T<:ImageWriter]:Resource[T]	= _.dispose()
	implicit def WindowResource[T<:Window]:Resource[T]				= _.dispose()
}
