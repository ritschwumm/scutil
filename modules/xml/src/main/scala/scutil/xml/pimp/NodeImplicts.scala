package scutil.xml.pimp

import scala.xml._

import scutil.xml._

object NodeImplicits extends NodeImplicits

trait NodeImplicits {
	implicit final class NodeExt(peer:Node) {
		// BETTER modify this?
		def xmlNamespaces:Seq[XmlNs]	= {
			def loop(scope:NamespaceBinding):Seq[XmlNs]	=
				if (scope == TopScope)	Vector.empty
				else					loop(scope.parent) :+ XmlNs(Option(scope.prefix), scope.uri)
			loop(peer.scope)
		}

		//------------------------------------------------------------------------------

		def asElem:Option[Elem]	=
			peer match {
				case x:Elem	=> Some(x)
				case _		=> None
			}

		def asText:Option[Text]	=
			peer match {
				case x:Text	=> Some(x)
				case _		=> None
			}

		def asPCData:Option[PCData]	=
			peer match {
				case x:PCData	=> Some(x)
				case _			=> None
			}

		def asUnparsed:Option[Unparsed]	=
			peer match {
				case x:Unparsed	=> Some(x)
				case _			=> None
			}


		def asComment:Option[Comment]	=
			peer match {
				case x:Comment	=> Some(x)
				case _			=> None
			}

		def asEntityRef:Option[EntityRef]	=
			peer match {
				case x:EntityRef	=> Some(x)
				case _				=> None
			}


		def asProcInstr:Option[ProcInstr]	=
			peer match {
				case x:ProcInstr	=> Some(x)
				case _			=> None
			}

		def asGroup:Option[Group]	=
			peer match {
				case x:Group	=> Some(x)
				case _			=> None
			}

		//------------------------------------------------------------------------------

		def toXhtml:String	= Xhtml toXhtml peer
	}
}
