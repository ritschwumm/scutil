package scutil.gui

import java.awt.Graphics
import javax.imageio.ImageReader
import javax.imageio.ImageWriter

import scutil.lang._

object disposables extends disposables
trait disposables {
	implicit def DisposableForGraphics(peer:Graphics):Disposable		= Disposable(peer.dispose _)
	implicit def DisposableForImageReader(peer:ImageReader):Disposable	= Disposable(peer.dispose _)
	implicit def DisposableForImageWriter(peer:ImageWriter):Disposable	= Disposable(peer.dispose _)
}
