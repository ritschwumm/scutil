package scutil.gui.extension

import java.awt.Toolkit
import java.awt.Image
import java.awt.image.ImageFilter
import java.awt.image.FilteredImageSource

object ImageExtensions {
	extension (peer:Image) {
		def filter(imageFilter:ImageFilter):Image	=
			Toolkit.getDefaultToolkit.createImage(
				new FilteredImageSource(
					peer.getSource,
					imageFilter
				)
			)
	}
}
