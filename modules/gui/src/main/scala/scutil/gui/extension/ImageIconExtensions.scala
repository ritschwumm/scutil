package scutil.gui.extension

import java.awt.Image
import java.awt.image.ImageFilter
import javax.swing.ImageIcon

import scutil.gui.extension.ImageExtensions.*

object ImageIconExtensions {
	implicit final class ImageIconExt(peer:ImageIcon) {
		def withImage(func:Image=>Image):ImageIcon	=
			new ImageIcon(func(peer.getImage))

		def filter(filter:ImageFilter):ImageIcon	=
			withImage { _ filter filter }
	}
}
