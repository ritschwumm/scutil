package scutil.pimp

import java.awt.Image
import javax.swing.ImageIcon

object ImageIconImplicits extends ImageIconImplicits

trait ImageIconImplicits {
	implicit def toImageIconExt(peer:ImageIcon) = new ImageIconExt(peer)
}

final class ImageIconExt(peer:ImageIcon) {
	def withImage(func:Image=>Image):ImageIcon	=
			new ImageIcon(func(peer.getImage))
}
