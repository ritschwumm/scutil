package scutil.gui

import scala.annotation.targetName

import java.awt.Graphics
import java.awt.Window
import javax.imageio.ImageReader
import javax.imageio.ImageWriter

import scala.util.Using.Releasable

object instances {
	@targetName("GraphicsReleasable")
	given [T<:Graphics]	=> Releasable[T]	= _.dispose()

	@targetName("ImageReaderReleasable")
	given [T<:ImageReader] => Releasable[T]	= _.dispose()

	@targetName("ImageWriterReleasable")
	given [T<:ImageWriter] => Releasable[T]	= _.dispose()

	@targetName("WindowReleasable")
	given [T<:Window] => Releasable[T]		= _.dispose()
}
