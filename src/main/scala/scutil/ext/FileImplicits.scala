package scutil.ext

import java.io._
import java.nio.charset.Charset

import annotation.tailrec

import scutil.Files
import scutil.Platform
import scutil.Resource._
import scutil.time.Instant

import AnyRefImplicits._
import BooleanImplicits._
import InputStreamImplicits._
import ReaderImplicits._

object FileImplicits extends FileImplicits

trait FileImplicits {
    implicit def toFileExt(delegate:File)	= new FileExt(delegate)
}

/** utility methods for java File objects */ 
final class FileExt(delegate:File) {
	private implicit def mkFileFilter(predicate:File=>Boolean) = new FileFilter {
		def accept(file:File):Boolean	= predicate(file)
	}
	
	// /** time of last modification as an Instant */
	// def lastModifiedInstant:Option[Instant]	=
	// 		delegate.lastModified guardBy { _ != 0 } map Instant.apply
	
	/** add a component to this Files's path */
	def /(name:String):File = new File(delegate, name)
	
	/** add a component to this Files's path */
	def /(path:Seq[String]):File	= path.foldLeft(delegate)(new File(_,_))
	
	/** names until getParentFile returns null */
	def path:List[String] = {
		@tailrec
		def unfold(file:File, path:List[String]):List[String] = 
				file.getParentFile match {
					case null	=> path
					case parent	=> unfold(parent, file.getName :: path)
				}
		unfold(delegate, Nil)
	}
	
	/** get the parent file the scala way */
	def parentFileOption:Option[File]	= 
			Option(delegate.getParentFile)
			
	/** get all parent directories starting with the root */
	def parentChain:List[File]	= {
		def unfold(file:File):List[File]	=
				file.getParentFile match {
					case null	=> Nil
					case parent	=> parent :: unfold(parent)
				}
		unfold(delegate)
	}
	
	/** list files in this directory */
	def children:Option[Seq[File]] =
			delegate.listFiles.guardNotNull map { _.toSeq }
			
	/** list files in this directory matching a predicate */
	def childrenWhere(predicate:File=>Boolean):Option[Seq[File]] =
			(delegate listFiles predicate).guardNotNull map { _.toSeq }
	
	/** Some existing file, or None */
	def guardExists:Option[File] =
			if (delegate.exists)	Some(delegate)
			else					None

	/** map only the name of this File */
	def modifyName(func:String=>String):File =
			new File(delegate.getParentFile, func(delegate.getName))
			
	/** copy this File over another */
	def copyTo(to:File) {
		 if (!to.exists)	to.createNewFile()
		 new FileInputStream(delegate).getChannel use { source =>
			 new FileOutputStream(to).getChannel use { target =>
				 target transferFrom (source, 0, source.size)
			 }
		 }
	}
	
	/** delete all children and the file itself */
	def deleteRecursive() {
		def recurse(file:File) {
			file.listFiles.guardNotNull.toSeq.flatten foreach recurse _
			file.delete()
		}
		recurse(delegate)
	}
	
	/** create a temp file within this directory */
	def createTempFile(prefix:String, suffix:String):File	=
			File createTempFile (prefix, suffix, delegate)
	
	/** create a temp directory within this directory */
	def createTempDirectory(prefix:String, suffix:String):File	= {
		val	file	= File createTempFile (prefix, suffix, delegate)
		require(file.delete(),	"cannot delete temp file: " + file)
		require(file.mkdir(),	"cannot create temp directory: " + file)
		file
	}
	
	//------------------------------------------------------------------------------
			
	/** execute a closure with an InputStream reading from this File */
	def withInputStream[T](code:(InputStream=>T)):T	=
			new FileInputStream(delegate) use code
	
	/** execute a closure with an OutputStream writing into this File */
	def withOutputStream[T](code:(OutputStream=>T)):T	=
			new FileOutputStream(delegate) use code
			
	/** execute a closure with a Reader reading from this File */
	def withReader[T](charset:Charset)(code:(Reader=>T)):T	=
			new InputStreamReader(new FileInputStream(delegate), charset) use code
	
	/** execute a closure with a Writer writing into this File */
	def withWriter[T](charset:Charset)(code:(Writer=>T)):T	=
			new OutputStreamWriter(new FileOutputStream(delegate), charset) use code

	//------------------------------------------------------------------------------
		
	def readBytes():Array[Byte]				= withInputStream { _ readFully () }
	def writeBytes(bytes:Array[Byte]):Unit	= withOutputStream { _ write bytes }
	
	def readString(charset:Charset):String					= withReader(charset) { _ readFully () }
	def writeString(charset:Charset, string:String):Unit	= withWriter(charset) { _ write string }
	
	// TODO writeLines should not use the platform line separator, readLines should honor a single lineSeparator 
	def readLines(charset:Charset):Seq[String]				= withReader(charset) { _ readLines () }
	def writeLines(charset:Charset, lines:Seq[String]):Unit	= withWriter(charset) { _ write (lines mkString Platform.line.separator) }
}