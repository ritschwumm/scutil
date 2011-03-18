package scutil

import java.io._
import java.nio.charset.Charset

import ext.FileImplicits._
import ext.InputStreamImplicits._
// import ext.OutputStreamImplicits._

final class ByteFile(delegate:File) {
	// TODO dry: get rid of withInputStream and withOutputStream
	
	/** execute a closure with an InputStream reading from this File */
	def withInputStream[T](code:(InputStream=>T)):T =
			delegate withInputStream code
	
	/** execute a closure with a Writer writing into this File */
	def withOutputStream[T](code:(OutputStream=>T)):T =
			delegate withOutputStream code
	
	/** read this File into a String */
	def read:Array[Byte]				= withInputStream { _ readFully () }
			
	/** write a String into this File */
	def write(bytes:Array[Byte]):Unit	= withOutputStream { _ write bytes }
}
