package scutil.pimp

import java.awt.Toolkit
import java.awt.Image
import java.awt.image.ImageFilter
import java.awt.image.FilteredImageSource

object ImageImplicits extends ImageImplicits

trait ImageImplicits {
	implicit def toImageExt(peer:Image) = new ImageExt(peer)
}

final class ImageExt(peer:Image) {
	def filter(imageFilter:ImageFilter):Image	=
			Toolkit.getDefaultToolkit createImage (
					new FilteredImageSource(
							peer.getSource,
							imageFilter))
}
