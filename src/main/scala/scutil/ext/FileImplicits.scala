package scutil.ext

import java.io._
import java.nio.charset.Charset

import annotation.tailrec

import scutil.Functions._
import scutil.Resource._
import scutil.Files
import scutil.Lists 
import scutil.Platform
import scutil.time.Instant

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
	
	//------------------------------------------------------------------------------
	//## file and directory
	
	/** time of last modification as an Instant, returns Instant.zero for non-existing files */
	def lastModifiedInstant:Instant	= 
			Instant(delegate.lastModified)
	
	/** add a component to this Files's path */
	def /(name:String):File 		= new File(delegate, name)
	
	/** add multiple components to this Files's path */
	def /(path:Seq[String]):File	= path.foldLeft(delegate)(new File(_,_))
	
	/** get the parent File the scala way */
	def parentFileOption:Option[File]	= 
			Option(delegate.getParentFile)
			
	/** get all parent Files starting with the immediate parent and ending with the directory root */
	def parentChain:List[File]	= 
			Lists unfoldRightSimple (
					delegate,
					(it:File) => Option(it.getParentFile))
					
	/** Some existing file, or None */
	def guardExists:Option[File] =
			if (delegate.exists)	Some(delegate)
			else					None

	/** map only the name of this File */
	def modifyName(func:String=>String):File =
			new File(delegate.getParentFile, func(delegate.getName))
	
	/** copy this File over another */
	def copyTo(to:File, force:Boolean=false) {
		 if (!to.exists) {
		 	to.createNewFile()
		 }
		 new FileInputStream(delegate).getChannel use { source =>
			 new FileOutputStream(to).getChannel use { target =>
			 	 var position	= 0L
			 	 while (position < source.size) {
			 	 	 position	+= (target transferFrom (source, 0, source.size-position))
				 }
				 if (force) {
				 	 target force true
				 }
			 }
		 }
	}
	
		/** walk this file and all children in pre order */
	def walkPreOrder(effect:Effect[File]) {
		def recurse(file:File) {
			effect(file)
			Option(file.listFiles).toSeq.flatten foreach recurse _
		}
		recurse(delegate)
	}
	
	/** walk this file and all children in post order */
	def walkPostOrder(effect:Effect[File]) {
		def recurse(file:File) {
			Option(file.listFiles).toSeq.flatten foreach recurse _
			effect(file)
		}
		recurse(delegate)
	}
	
	/** delete all children and the file itself */
	def deleteRecursive() {
		walkPostOrder(_.delete())
		/*
		def recurse(file:File) {
			Option(file.listFiles).toSeq.flatten foreach recurse _
			file.delete()
		}
		recurse(delegate)
		*/
	}

	//------------------------------------------------------------------------------
	//## directory only
	
	/** list files in this directory */
	def children:Option[Seq[File]] =
			Option(delegate.listFiles) map { _.toSeq }
			
	/** list files in this directory matching a predicate */
	def childrenWhere(predicate:File=>Boolean):Option[Seq[File]] =
			Option(delegate listFiles predicate) map { _.toSeq }
			
	//------------------------------------------------------------------------------
	//## file only: resource closure
	
	/** execute a closure with an InputStream reading from this File */
	def withInputStream[T](code:(FileInputStream=>T)):T	=
			new FileInputStream(delegate) use code
	
	/** execute a closure with an OutputStream writing into this File */
	def withOutputStream[T](code:(FileOutputStream=>T)):T	=
			new FileOutputStream(delegate) use code
			
	/** execute a closure with a Reader reading from this File */
	def withReader[T](charset:Charset)(code:(InputStreamReader=>T)):T	=
			new InputStreamReader(new FileInputStream(delegate), charset) use code
	
	/** execute a closure with a Writer writing into this File */
	def withWriter[T](charset:Charset)(code:(OutputStreamWriter=>T)):T	=
			new OutputStreamWriter(new FileOutputStream(delegate), charset) use code

	//------------------------------------------------------------------------------
	//## file only: complete read
	
	def readBytes():Array[Byte]				= withInputStream { _ readFully () }
	def writeBytes(bytes:Array[Byte]):Unit	= withOutputStream { _ write bytes }
	
	def readString(charset:Charset):String					= withReader(charset) { _ readFully () }
	def writeString(charset:Charset, string:String):Unit	= withWriter(charset) { _ write string }
	
	// TODO writeLines should not use the platform line separator, readLines should honor a single lineSeparator 
	def readLines(charset:Charset):Seq[String]	= 
			withReader(charset) { _ readLines () }
	def writeLines(charset:Charset, lines:Seq[String]):Unit	= 
			withWriter(charset) { writer => 
				lines foreach { line => 
					writer write line
					writer write Platform.line.separator
				}
			}
			
	//------------------------------------------------------------------------------
	//## temp
	
	/** create a temp file within this directory */
	def createTempFile(prefix:String, suffix:String = null):File	=
			File createTempFile (prefix, suffix, delegate)
	
	/** create a temp directory within this directory */
	def createTempDirectory(prefix:String, suffix:String = null):File	= {
		require(prefix.length >= 3, "prefix must be at least 3 characters long")
		val	file	= File createTempFile (prefix, suffix, delegate)
		require(file.delete(),	"cannot delete temp file: " + file)
		require(file.mkdir(),	"cannot create temp directory: " + file)
		file
	}
}
