package scutil.gui

import java.awt.Graphics
import java.awt.Window
import javax.imageio.ImageReader
import javax.imageio.ImageWriter

import scala.util.Using.Releasable

object instances {
	given GraphicsReleasable[T<:Graphics]:Releasable[T]			= _.dispose()
	given ImageReaderReleasable[T<:ImageReader]:Releasable[T]	= _.dispose()
	given ImageWriterReleasable[T<:ImageWriter]:Releasable[T]	= _.dispose()
	given WindowReleasable[T<:Window]:Releasable[T]				= _.dispose()
}
