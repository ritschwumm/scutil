package scutil.pimp

import scala.xml._

object NodeImplicits extends NodeImplicits

trait NodeImplicits {
    implicit def toNodeExt(delegate:Node)	= new NodeExt(delegate)
}

final class NodeExt(delegate:Node) {
	def toXhtml:String	= Xhtml toXhtml delegate
}
