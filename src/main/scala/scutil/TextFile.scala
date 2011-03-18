package scutil

import java.io._
import java.nio.charset.Charset

import ext.FileImplicits._
import ext.ReaderImplicits._
import ext.WriterImplicits._

final class TextFile(delegate:File, charset:Charset) {
	// TODO dry: get rid of withReader and withWriter

	/** execute a closure with a Reader reading from this File */
	def withReader[T](code:(Reader=>T)):T =
			delegate.withReader(charset)(code)
	
	/** execute a closure with a Writer writing into this File */
	def withWriter[T](code:(Writer=>T)):T =
			delegate.withWriter(charset)(code)
	
	/** read this File into a String */
	def read:String				= withReader { _ readFully () }
			
	/** write a String into this File */
	def write(text:String):Unit	= withWriter { _ write text }
	
	/** read this File into a sequence of Lines */
	def readLines:Seq[String]				= withReader { _ readLines () }
	
	/** write a sequence of lines into this File */
	def writeLines(lines:Seq[String]):Unit	= withWriter { _ writeLines lines }
}
