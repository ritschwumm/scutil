package scutil.ext

import java.io._
import java.nio.charset.Charset

import annotation.tailrec

import scutil.Files
import scutil.TextFile
import scutil.ByteFile
import scutil.Resource._

import AnyRefImplicits._
import BooleanImplicits._

object FileImplicits extends FileImplicits

trait FileImplicits {
    implicit def toFileExt(delegate:File)	= new FileExt(delegate)
}

/** utility methods for java File objects */ 
final class FileExt(delegate:File) {
	/** add a component to this Files's path */
	def /(name:String):File = new File(delegate, name)
	
	/** add a component to this Files's path */
	def /(path:Seq[String]):File	= path.foldLeft(delegate)(new File(_,_))
	
	/** names until getParentFile returns null */
	def path:List[String] = {
		@tailrec def recurse(file:File, path:List[String]):List[String] = file.getParentFile match {
			case null	=> path
			case parent	=> recurse(parent, file.getName :: path)
		}
		recurse(delegate, Nil)
	}
	
	/** get the parent file the scala way */
	def parentFileOption:Option[File]	= 
			Option(delegate.getParentFile)
			
	/** get all parent directories starting with the root */
	def parentChain:List[File]	= {
		// TODO this is an unfold!
		def recurse(file:File):List[File]	=
				file.getParentFile match {
					case null	=> Nil
					case parent	=> parent :: recurse(parent)
				}
		recurse(delegate)
	}
	
	/** list files in this directory matching a predicate */
	def childrenWhere(predicate:File=>Boolean):Option[Seq[File]] =
			(delegate listFiles (Files mkFileFilter predicate)).nullOption map { _.toSeq }
	
	/** Some existing file, or None */
	def existsOption:Option[File] = 
			delegate.exists guard delegate

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
			Option(file.listFiles).toSeq.flatten foreach recurse _
			file.delete()
		}
		recurse(delegate)
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
			
	def toByteFile:ByteFile						= new ByteFile(delegate)
	def toTextFile(charset:Charset):TextFile	= new TextFile(delegate, charset)
}
