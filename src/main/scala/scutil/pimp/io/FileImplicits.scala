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
    implicit def toFileExt(peer:File)	= new FileExt(peer)
}

/** utility methods for java File objects */ 
final class FileExt(peer:File) {
	private implicit def mkFileFilter(predicate:File=>Boolean) =
			new PredicateFileFilter(predicate)
	
	//------------------------------------------------------------------------------
	//## file and directory
	
	/** time of last modification as an MilliInstant, returns MilliInstant.zero for non-existing files */
	def lastModifiedMilliInstant:MilliInstant	= 
			MilliInstant(peer.lastModified)
		
	def updateLastModifiedMilliInstant(it:MilliInstant):Boolean	=
			peer setLastModified it.millis
	
	/** whether the peer is newer than another file. if the other file does not exist it counts as newer */ 
	def newerThan(that:File):Boolean	=
			peer.exists && (!that.exists || peer.lastModified > that.lastModified)
	
	/** add a component to this Files's path */
	def /(name:String):File 		= new File(peer, name)
	
	/** add multiple components to this Files's path */
	def /+(path:Seq[String]):File	= (path foldLeft peer) { new File(_,_) }
	
	/** get the parent File the scala way */
	def parentOption:Option[File]	= 
			Option(peer.getParentFile)
			
	/** get all parent Files starting with the immediate parent and ending with the directory root */
	def parentChain:List[File]	= 
			Lists unfoldRightSimple (
					peer,
					(it:File) => Option(it.getParentFile))
	
	/** like parentChain but includes the starting file itself */
	def selfAndParentChain:List[File]	=
			peer :: parentChain
					
	/** Some existing file, or None */
	def guardExists:Option[File] =
			if (peer.exists)	Some(peer)
			else				None

	/** map only the name of this File */
	def modifyName(func:String=>String):File =
			new File(peer.getParentFile, func(peer.getName))
	
	//------------------------------------------------------------------------------
	//## directory only
	
	/** list files in this directory */
	def children:Option[Seq[File]] =
			Option(peer.listFiles) map { _.toVector }
			
	/** list files in this directory matching a predicate */
	def childrenWhere(predicate:File=>Boolean):Option[Seq[File]] =
			Option(peer listFiles predicate) map { _.toVector }
		
	/** the path upwards from another File to this File */
	def containsRecursive(that:File):Option[Seq[String]]	= {
		def loop(test:File, path:Seq[String]):Option[Seq[String]]	=
					 if (test == null)	None
				else if (test == peer)	Some(path)
				else					loop(test.getParentFile, test.getName +: path)
		loop(that, Vector.empty)
	}
			
	//------------------------------------------------------------------------------
	//## file only: streams
	
	def openInputStream():Tried[FileNotFoundException,InputStream]	=
			Catch.byType[FileNotFoundException] in new FileInputStream(peer)
			
	def openOutputStream():Tried[FileNotFoundException,OutputStream]	=
			Catch.byType[FileNotFoundException] in new FileOutputStream(peer)
			
	//------------------------------------------------------------------------------
	//## file only: resource closure
	
	// TODO handle IOException
	
	/** execute a closure with an InputStream reading from this File */
	def withInputStream[T](code:(FileInputStream=>T)):T	=
			new FileInputStream(peer) use code
	
	/** execute a closure with an OutputStream writing into this File */
	def withOutputStream[T](code:(FileOutputStream=>T)):T	=
			new FileOutputStream(peer) use code
			
	/** execute a closure with a Reader reading from this File */
	def withReader[T](charset:Charset)(code:(InputStreamReader=>T)):T	=
			new InputStreamReader(new FileInputStream(peer), charset) use code
	
	/** execute a closure with a Writer writing into this File */
	def withWriter[T](charset:Charset)(code:(OutputStreamWriter=>T)):T	=
			new OutputStreamWriter(new FileOutputStream(peer), charset) use code

	//------------------------------------------------------------------------------
	//## file only: complete read
	
	// TODO handle IOException
	
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
		 new FileInputStream(peer).getChannel use { source =>
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
		def loop(file:File) {
			val deleted	= file.delete()
			// NOTE this prevents deletion of the contents of symlinked directories
			if (!deleted && file.isDirectory) {
				val list	= file.listFiles
				if (list != null) {
					list foreach loop
				}
				file.delete()
			}
		}
		loop(peer)
	}
	
	//------------------------------------------------------------------------------
	//## temp
	
	/** create a temp file within this directory */
	def createTempFile(prefix:String, suffix:String = null):File	= {
		require(prefix.length >= 3, "prefix must be at least 3 characters long")
		File createTempFile (prefix, suffix, peer)
	}
	
	/** create a temp directory within this directory */
	def createTempDirectory(prefix:String, suffix:String = null):File	= {
		require(prefix.length >= 3, "prefix must be at least 3 characters long")
		val	file	= File createTempFile (prefix, suffix, peer)
		require(file.delete(),	"cannot delete temp file: " + file)
		require(file.mkdir(),	"cannot create temp directory: " + file)
		file
	}
}

private final class PredicateFileFilter(predicate:Predicate[File]) extends FileFilter {
	def accept(file:File):Boolean	= predicate(file)
}
