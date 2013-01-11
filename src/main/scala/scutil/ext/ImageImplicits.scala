package scutil.ext

import java.awt.Toolkit
import java.awt.Image
import java.awt.image.ImageFilter
import java.awt.image.FilteredImageSource

object ImageImplicits extends ImageImplicits

trait ImageImplicits {
	implicit def toImageExt(delegate:Image) = new ImageExt(delegate)
}

final class ImageExt(delegate:Image) {
	def filter(imageFilter:ImageFilter):Image	=
			Toolkit.getDefaultToolkit createImage (
					new FilteredImageSource(
							delegate.getSource,
							imageFilter))
}
