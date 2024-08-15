package scutil.gui.extension

import java.awt.Image
import java.awt.image.ImageFilter
import javax.swing.ImageIcon

object ImageIconExtensions {
	extension (peer:ImageIcon) {
		def withImage(func:Image=>Image):ImageIcon	=
			new ImageIcon(func(peer.getImage))

		def filter(filter:ImageFilter):ImageIcon	=
			withImage { ImageExtensions.filter(_)(filter) }
	}
}
