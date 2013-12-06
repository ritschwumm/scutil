package scutil.pimp

import scala.xml._

object NodeImplicits extends NodeImplicits

trait NodeImplicits {
    implicit def toNodeExt(peer:Node)	= new NodeExt(peer)
}

final class NodeExt(peer:Node) {
	def toXhtml:String	= Xhtml toXhtml peer
}
