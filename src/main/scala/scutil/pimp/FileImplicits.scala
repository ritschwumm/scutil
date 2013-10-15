package scutil.pimp

import java.io._
import java.nio.charset.Charset

import scutil.lang._
import scutil.pimp.AnyImplicits._
import scutil.pimp.ReaderImplicits._
import scutil.pimp.InputStreamImplicits._
import scutil.pimp.DisposableConversions._
import scutil.io.Files
import scutil.time.MilliInstant
import scutil.platform.SystemProperties

object FileImplicits extends FileImplicits

trait FileImplicits {
    implicit def toFileExt(delegate:File)	= new FileExt(delegate)
}

/** utility methods for java File objects */ 
final class FileExt(delegate:File) {
	private implicit def mkFileFilter(predicate:File=>Boolean) =
			new PredicateFileFilter(predicate)
	
	//------------------------------------------------------------------------------
	//## file and directory
	
	/** time of last modification as an MilliInstant, returns MilliInstant.zero for non-existing files */
	def lastModifiedMilliInstant:MilliInstant	= 
			MilliInstant(delegate.lastModified)
	
	/** whether the delegate is newer than another file. if the other file does not exist it counts as newer */ 
	def newerThan(that:File):Boolean	=
			delegate.exists && (!that.exists || delegate.lastModified > that.lastModified)
	
	/** add a component to this Files's path */
	def /(name:String):File 		= new File(delegate, name)
	
	/** add multiple components to this Files's path */
	def /+(path:Seq[String]):File	= (path foldLeft delegate) { new File(_,_) }
	
	/** get the parent File the scala way */
	def parentOption:Option[File]	= 
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
	
	//------------------------------------------------------------------------------
	//## directory only
	
	/** list files in this directory */
	def children:Option[Seq[File]] =
			Option(delegate.listFiles) map { _.toSeq }
			
	/** list files in this directory matching a predicate */
	def childrenWhere(predicate:File=>Boolean):Option[Seq[File]] =
			Option(delegate listFiles predicate) map { _.toSeq }
		
	/** the path upwards from another File to this File */
	def containsRecursive(that:File):Option[Seq[String]]	= {
		def loop(test:File, path:Seq[String]):Option[Seq[String]]	=
					 if (test == null)		None
				else if (test == delegate)	Some(path)
				else						loop(test.getParentFile, test.getName +: path)
		loop(that, Seq.empty)
	}
			
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
	
	def readBytes():Array[Byte]				= withInputStream	{ _ readFully ()	}
	def writeBytes(bytes:Array[Byte]):Unit	= withOutputStream	{ _ write bytes		}
	
	def readString(charset:Charset):String					= withReader(charset) { _ readFully () }
	def writeString(charset:Charset, string:String):Unit	= withWriter(charset) { _ write string }
	
	// TODO should honor a single line separator 
	def readLines(charset:Charset):Seq[String]	= 
			withReader(charset) { _ readLines () }
	// TODO should not use the platform line separator
	def writeLines(charset:Charset, lines:Seq[String]):Unit	= 
			withWriter(charset) { writer => 
				lines foreach { line => 
					writer write line
					writer write SystemProperties.line.separator
				}
			}
			
	//------------------------------------------------------------------------------
	//## manipulation
	
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
	
	/** delete all children and the file itself */
	def deleteRecursive() {
		def recurse(file:File) {
			val deleted	= file.delete()
			// NOTE this prevents deletion of the contents of symlinked directories
			if (!deleted && file.isDirectory) {
				val list	= file.listFiles
				if (list != null) {
					list foreach recurse
				}
				file.delete()
			}
		}
		recurse(delegate)
	}
	
	//------------------------------------------------------------------------------
	//## temp
	
	/** create a temp file within this directory */
	def createTempFile(prefix:String, suffix:String = null):File	= {
		require(prefix.length >= 3, "prefix must be at least 3 characters long")
		File createTempFile (prefix, suffix, delegate)
	}
	
	/** create a temp directory within this directory */
	def createTempDirectory(prefix:String, suffix:String = null):File	= {
		require(prefix.length >= 3, "prefix must be at least 3 characters long")
		val	file	= File createTempFile (prefix, suffix, delegate)
		require(file.delete(),	"cannot delete temp file: " + file)
		require(file.mkdir(),	"cannot create temp directory: " + file)
		file
	}
}

private final class PredicateFileFilter(predicate:Predicate[File]) extends FileFilter {
	def accept(file:File):Boolean	= predicate(file)
}
