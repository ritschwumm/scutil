package scutil.xml.pimp

import scala.collection.immutable.{ Seq => ISeq }
import scala.xml._

import scutil.lang._
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
		
		def xmlAttrs:ISeq[(XmlAttr, String)]	=
				peer.attributes.toVector map {
					case it:PrefixedAttribute	=> XmlAttr(Some(it.pre),	it.key)	-> it.value.text
					case it:UnprefixedAttribute	=> XmlAttr(None, 			it.key)	-> it.value.text
				}
				
		def updateXmlAttrs(attrs:ISeq[(XmlAttr, String)]):Elem	=
				peer copy (attributes =
					(attrs foldLeft (Null:MetaData)) { (md, next) =>
						next match {
							case (XmlAttr(pre, key), value) => Attribute(pre.orNull, key, value, md)
						}
					}
				)
	
		def modifyXmlAttrs(func:Endo[ISeq[(XmlAttr, String)]]):Elem	=
				updateXmlAttrs(func(xmlAttrs))
				
		def updateChildren(children:ISeq[Node]):Elem	=
				peer copy (child = children)
			
		def modifyChildren(func:Endo[ISeq[Node]]):Elem	=
				updateChildren(func(peer.child))
			
		//------------------------------------------------------------------------------
			
		def toStringWithDeclaration:String	=
				"""<?xml version="1.0" encoding="utf-8"?>""" + "\n" +
				peer.toString
	}
}
