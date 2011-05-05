package scutil.ext

import java.io._

import scala.collection.mutable

object ReaderImplicits extends ReaderImplicits

trait ReaderImplicits {
    implicit def toReaderExt(delegate:Reader)	= new ReaderExt(delegate)
}

/** utility methods for Reader objects */ 
final class ReaderExt(delegate:Reader) {
	val blockSize	= 16384
	
	def readFully():String = {
		val	buffer	= new Array[Char](blockSize)
		val out		= new StringBuilder
		var	running	= true
		while (running) {
			val len	= delegate read buffer
			if (len != -1)	out appendAll (buffer, 0, len)
			else			running	= false
		}
		out.toString
	}
	
	// TODO should take a line separator argument
	def readLines():Seq[String]	= {
		val in		= new BufferedReader(delegate)
		val buffer	= new mutable.ArrayBuffer[String]
		var running	= true
		while (running) {
			val	line	= in.readLine
			if (line != null)	buffer += line
			else				running		= false
		}
		buffer
	}
	
	def copyTo(out:Writer) {
		val	buffer	= new Array[Char](blockSize)
		var running	= true
		while (running) {
			val	len	= delegate read buffer
			if (len != -1)	out write (buffer, 0, len)
			else			running	= false
		}
	}
	
	def drain() {
		val	buffer	= new Array[Char](blockSize)
		var running	= true
		while (true) {
			val len = delegate read buffer
			if (len == -1)	running	= false
		}
	}
}
