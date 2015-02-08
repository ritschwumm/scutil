package scutil.extra.pimp

import scala.xml.Elem

object ElemImplicits extends ElemImplicits

trait ElemImplicits {
	implicit def toElemExt(peer:Elem) = new ElemExt(peer)
}

final class ElemExt(peer:Elem) {
	def toStringWithDeclaration:String	=
			"""<?xml version="1.0" encoding="utf-8"?>""" + "\n" +
			peer.toString
}
