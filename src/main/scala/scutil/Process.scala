package scutil

import java.io._

import scala.collection.mutable
import scala.collection.JavaConversions

import scutil.lang._
import scutil.Resources._
import scutil.ext.ReaderImplicits._
import scutil.ext.ExecutorImplicits._

object Process {
	case class Result(rc:Int, out:Seq[String], err:Seq[String])
	
	def exec(command:String*):Result = 
			exec(command, Map.empty[String,String], None, Seq.empty[String])
		
	def exec(command:Seq[String], env:Map[String,String], pwd:Option[File], input:Seq[String]):Result = {
		import JavaConversions._
		
		val	builder	= new ProcessBuilder(command)
		builder.environment() putAll env
		pwd foreach { builder directory _ }
		val proc	= builder.start()
		
		val in	= spawn { spewLines(proc.getOutputStream, input) }
		val	err	= spawn { slurpLines(proc.getErrorStream) }
		val	out	= spawn { slurpLines(proc.getInputStream) }
		// avoid memory leak, see http://developer.java.sun.com/developer/qow/archive/68/
		proc.waitFor()
		val	rc	= proc.exitValue
		Result(rc, out(), err())
	}
	
	private def slurpLines(st:InputStream):Seq[String] = {
		new InputStreamReader(st) use { _.readLines() }
	}
	
	private def spewLines(st:OutputStream, lines:Seq[String]) {
		new OutputStreamWriter(st) use { writer =>
			lines foreach { line =>
				writer write line
				writer write SystemProperties.line.separator
			}
		}
	}
	
	private def spawn[T](task: =>T):Thunk[T] =
			Executors.spawn withResult thunk(task)
}
