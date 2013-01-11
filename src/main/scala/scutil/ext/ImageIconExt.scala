package scutil.ext

import java.awt.Image
import javax.swing.ImageIcon

object ImageIconImplicits extends ImageIconImplicits

trait ImageIconImplicits {
	implicit def toImageIconExt(delegate:ImageIcon) = new ImageIconExt(delegate)
}

final class ImageIconExt(delegate:ImageIcon) {
	def withImage(func:Image=>Image):ImageIcon	=
			new ImageIcon(func(delegate.getImage))
}
