package scutil.gui

import java.awt.Graphics
import java.awt.Window
import javax.imageio.ImageReader
import javax.imageio.ImageWriter

import scala.util.Using.Releasable

object instances extends instances

trait instances {
	implicit def GraphicsReleasable[T<:Graphics]:Releasable[T]			= _.dispose()
	implicit def ImageReaderReleasable[T<:ImageReader]:Releasable[T]	= _.dispose()
	implicit def ImageWriterReleasable[T<:ImageWriter]:Releasable[T]	= _.dispose()
	implicit def WindowReleasable[T<:Window]:Releasable[T]				= _.dispose()
}
