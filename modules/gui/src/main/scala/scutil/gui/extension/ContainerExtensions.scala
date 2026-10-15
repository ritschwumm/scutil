package scutil.gui.extension

import java.awt.{ List as _, * }

object ContainerExtensions {
	extension (peer:Container) {
		def children:Seq[Component]	= childrenIterator.toVector

		def childrenIterator:Iterator[Component]	=
			new Iterator[Component] {
				var	index	= 0
				def hasNext:Boolean		= index < peer.getComponentCount
				def next():Component	= {
					val	out	= peer.getComponent(index)
					index	+= 1
					out
				}
			}
	}
}
