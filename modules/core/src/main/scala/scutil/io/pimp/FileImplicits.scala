package scutil.io.pimp

import java.io._
import java.nio.file.Files
import java.nio.charset.Charset

import scutil.lang._
import scutil.base.implicits._
import scutil.io.implicits._
import scutil.time._
import scutil.platform.SystemProperties

object FileImplicits extends FileImplicits

trait FileImplicits {
	/** utility methods for java File objects */
	implicit final class FileExt(peer:File) {
		private implicit def mkFileFilter(predicate:File=>Boolean) =
				new PredicateFileFilter(predicate)
		
		//------------------------------------------------------------------------------
		//## file and directory
		
		/** time of last modification as an MilliInstant, returns MilliInstant.zero for non-existing files */
		def lastModifiedMilliInstant():MilliInstant	=
				MilliInstant(peer.lastModified)
			
		def setLastModifiedMilliInstant(it:MilliInstant):Boolean	=
				peer setLastModified it.millis
		
		/** whether the peer is newer than another file. if the other file does not exist it counts as newer */
		def newerThan(that:File):Boolean	=
				peer.exists && (!that.exists || peer.lastModified > that.lastModified)
		
		/** add a component to this Files's path */
		def /(name:String):File 		= new File(peer, name)
		
		/** add multiple components to this Files's path */
		def /+(path:ISeq[String]):File	= (path foldLeft peer) { new File(_,_) }
		
		/** get the parent File the scala way */
		def parentOption:Option[File]	=
				Option(peer.getParentFile)
				
		/** get all parent Files starting with the immediate parent and ending with the directory root */
		def parentChain:List[File]	=
				List unfoldRightSimple (
					peer,
					(it:File) => Option(it.getParentFile)
				)
		
		/** like parentChain but includes the starting file itself */
		def selfAndParentChain:List[File]	=
				peer :: parentChain
						
		/** Some existing file, or None */
		def optionExists:Option[File] =
				if (peer.exists)	Some(peer)
				else				None
	
		/** another file in the same parent directory */
		def sibling(name:String):File =
				new File(peer.getParentFile, name)
		
		/** map only the name of this File */
		def siblingBy(func:String=>String):File =
				sibling(func(peer.getName))
		
		//------------------------------------------------------------------------------
		//## directory only
		
		/** list files in this directory */
		def children:Option[ISeq[File]] =
				Option(peer.listFiles) map { _.toVector }
				
		/** list files in this directory matching a predicate */
		def childrenWhere(predicate:File=>Boolean):Option[ISeq[File]] =
				Option(peer listFiles predicate) map { _.toVector }
			
		/** the path upwards from another File to this File */
		def containsRecursive(that:File):Option[ISeq[String]]	= {
			def loop(test:File, path:ISeq[String]):Option[ISeq[String]]	=
						 if (test == null)	None
					else if (test == peer)	Some(path)
					else					loop(test.getParentFile, test.getName +: path)
			loop(that, Vector.empty)
		}
		
		//------------------------------------------------------------------------------
		//## file only: streams
		
		def newInputStream():InputStream	=
				Files newInputStream peer.toPath
		
		def newOutputStream():OutputStream	=
				Files newOutputStream peer.toPath
		
		//------------------------------------------------------------------------------
		//## file only: resource closure
		
		// BETTER handle IOException
		
		/** execute a closure with an InputStream reading from this File */
		def withInputStream[T](code:(InputStream=>T)):T	=
				newInputStream() use code
		
		/** execute a closure with an OutputStream writing into this File */
		def withOutputStream[T](code:(OutputStream=>T)):T	=
				newOutputStream() use code
				
		/** execute a closure with a Reader reading from this File */
		def withReader[T](charset:Charset)(code:(InputStreamReader=>T)):T	=
				new InputStreamReader(newInputStream(), charset) use code
		
		/** execute a closure with a Writer writing into this File */
		def withWriter[T](charset:Charset)(code:(OutputStreamWriter=>T)):T	=
				new OutputStreamWriter(newOutputStream(), charset) use code
	
		//------------------------------------------------------------------------------
		//## file only: complete read
		
		// TODO Files readAllBytes delegate
		// TODO Files write (delegate, content)
		def readByteString():ByteString				= withInputStream	{ _ readFullyByteString ()	}
		def writeByteString(bytes:ByteString):Unit	= withOutputStream	{ _ writeByteString bytes	}
		
		def readString(charset:Charset):String					= withReader(charset) { _ readFully () }
		def writeString(charset:Charset, string:String):Unit	= withWriter(charset) { _ write string }
		
		// BETTER  use a specific line separator
		def readLines(charset:Charset):ISeq[String]	=
				withReader(charset) { _ readLines () }
		def writeLines(charset:Charset, lines:ISeq[String]):Unit	=
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
			require(file.delete(),	"cannot delete temp file: " + file.toString)
			require(file.mkdir(),	"cannot create temp directory: " + file.toString)
			file
		}
	}
}
