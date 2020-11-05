package scutil.xml.extension

import scala.xml._

import scutil.xml._

object ElemImplicits extends ElemImplicits

trait ElemImplicits {
	implicit final class ElemExt(peer:Elem) {
		def attributeValue(attr:XmlAttr):Option[String]	=
			peer.attributes
			.find	{
				case x:Attribute	=>
					x.pre == attr.prefix.orNull &&
					x.key == attr.key
				case _	=> false
			}
			.map	{ _.value.text }

		def xmlAttrs:Seq[(XmlAttr, String)]	=
			peer.attributes.toVector map {
				case it:PrefixedAttribute	=> XmlAttr(Some(it.pre),	it.key)	-> it.value.text
				case it:UnprefixedAttribute	=> XmlAttr(None, 			it.key)	-> it.value.text
			}

		def updateXmlAttrs(attrs:Seq[(XmlAttr, String)]):Elem	=
			peer copy (attributes =
				(attrs foldLeft (Null:MetaData)) { (md, next) =>
					next match {
						case (XmlAttr(pre, key), value) => Attribute(pre.orNull, key, value, md)
					}
				}
			)

		def modifyXmlAttrs(func:Seq[(XmlAttr, String)]=>Seq[(XmlAttr, String)]):Elem	=
			updateXmlAttrs(func(xmlAttrs))

		def updateChildren(children:Seq[Node]):Elem	=
			peer copy (child = children)

		def modifyChildren(func:Seq[Node]=>Seq[Node]):Elem	=
			updateChildren(func(peer.child))

		//------------------------------------------------------------------------------

		def toStringWithDeclaration:String	=
			"""<?xml version="1.0" encoding="utf-8"?>""" + "\n" +
			peer.toString
	}
}
