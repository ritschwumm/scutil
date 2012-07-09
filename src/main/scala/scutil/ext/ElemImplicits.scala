package scutil.ext

import scala.xml.Elem

object ElemImplicits extends ElemImplicits

trait ElemImplicits {
	implicit def toElemExt(delegate:Elem) = new ElemExt(delegate)
}

final class ElemExt(delegate:Elem) {
	def toStringWithDeclaration:String	= 
			"""<?xml version="1.0" encoding="utf-8"?>""" + "\n" + 
			delegate
}
