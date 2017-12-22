package scutil.gui.pimp

import java.awt.Image
import java.awt.image.ImageFilter
import javax.swing.ImageIcon

import scutil.gui.pimp.ImageImplicits._

object ImageIconImplicits extends ImageIconImplicits

trait ImageIconImplicits {
	implicit final class ImageIconExt(peer:ImageIcon) {
		def withImage(func:Image=>Image):ImageIcon	=
				new ImageIcon(func(peer.getImage))
			
		def filter(filter:ImageFilter):ImageIcon	=
				withImage { _ filter filter }
	}
}
