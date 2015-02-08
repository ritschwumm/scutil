package scutil.gui

import java.awt.Graphics
import javax.imageio.ImageReader
import javax.imageio.ImageWriter
import javax.imageio.stream.ImageInputStream

import scutil.lang._

object disposables extends disposables
trait disposables {
	implicit def DisposableForImageReader		(peer:ImageReader)		= Disposable(peer.dispose)
	implicit def DisposableForImageWriter		(peer:ImageWriter)		= Disposable(peer.dispose)
	implicit def DisposableForImageInputStream	(peer:ImageInputStream)	= Disposable(peer.close)
	implicit def DisposableForGraphics			(peer:Graphics)			= Disposable(peer.dispose)
}
