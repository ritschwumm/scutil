package scutil.xml

import java.io._

import javax.xml.parsers.SAXParserFactory

import scala.xml.Elem
import scala.xml.Node
import scala.xml.Utility
import scala.xml.MinimizeMode
import scala.xml.dtd.DocType
import scala.xml.factory.XMLLoader
import javax.xml.parsers.SAXParser

import scutil.base.implicits._
import scutil.core.implicits._
import scutil.lang.Charsets

/*
NOTE
-	XML.load* accesses external DTDs and such
-	ConstructingParser hiccups when it sees minimal doctypes like <!DOCTYPE xmeml>
-	setting http://apache.org/xml/features/disallow-doctype-dec to false in the
	SAXParserFactory doesn't allow any doctype
-	setting the Validating property of a SaxParser to false does what???
-	@see http://stackoverflow.com/questions/295766/java-sax-parser-raises-unknownhostexception
-	@see http://stackoverflow.com/questions/243728/how-to-disable-dtd-at-runtime-in-javas-xpath
*/

/**
disable doctypes checking:
-	parser must not fetch urls mentioned there
-	scala.xml doesn't like short doctypes like <!DOCTYPE xmeml>
@see https://issues.scala-lang.org/browse/SI-2725
*/
object FixedXML extends XMLLoader[Elem] {
	val encoding	= Charsets.utf_8

	/*
	def loadXML(file:File):Node =
			loadXML(Source fromFile file)

	def loadXML(string:String):Node	=
			loadXML(Source fromString string)

	def loadXML(source:Source):Node = {
		val prs	= ConstructingParser fromSource (source, true)
		val	doc	= prs.document
		require(doc != null, "cannot parse XML file: " + source)
		doc.docElem
	}

	def saveXML(file:File, node:Node) {
		file writeString (encoding, node.toString)
	}
	*/

	def saveFile(file:File, node:Node, xmlDecl:Boolean=true, docType:Option[DocType]=None, minimizeTags:MinimizeMode.Value=MinimizeMode.Default):Unit = {
		(file withWriter encoding) { write(node, xmlDecl, docType, minimizeTags) }
	}

	private def write(node:Node, xmlDecl:Boolean, docType:Option[DocType], minimizeTags:MinimizeMode.Value)(writer:Writer):Unit = {
		xmlDecl option s"<?xml version='1.0' encoding='${encoding.name}'?>\n" foreach writer.write
		docType map { _.toString + "\n" } foreach writer.write
		writer write (Utility serialize (node, minimizeTags = minimizeTags)).toString
	}

	override def parser:SAXParser	= {
		val f	= SAXParserFactory.newInstance()
		f	setNamespaceAware	false

		// ignore doctype
		f	setValidating		false

		// do not load external stuff
		f	setFeature			("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)

		// disallow doctype
		// f	setFeature		("http://apache.org/xml/features/disallow-doctype-decl", true)

		f.newSAXParser()
	}
}
