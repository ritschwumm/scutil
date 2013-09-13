package scutil.pimp

import java.awt.{ List=>AwtList, _ }
import javax.swing._

object ContainerImplicits extends ContainerImplicits

trait ContainerImplicits {
	implicit def toContainerExt(delegate:Container):ContainerExt	= new ContainerExt(delegate)
}
	
final class ContainerExt(delegate:Container) {
	def children:Seq[Component]	= childrenIterator.toSeq
	
	def childrenIterator:Iterator[Component]	= 
			new Iterator[Component] {
				var	index	= 0
				def hasNext:Boolean		= index < delegate.getComponentCount
				def next():Component	= {
					val	out	= delegate getComponent index
					index	+= 1
					out
				}
			}
}
