package scutil.platform

import java.io.*
import java.nio.file.Path

import scutil.lang.*
import scutil.core.implicits.*
import scutil.jdk.implicits.*
import scutil.concurrent.*

object External {
	/** execute an external process. */
	def exec(command:Seq[String], env:Map[String,String]=Map.empty, pwd:Option[Path]=None, input:Seq[String]=Seq.empty):External = {
		val	builder	= new ProcessBuilder(command.toJList)
		builder.environment() putAll env.toJMap
		pwd foreach { path => builder.directory(path.toFile) }
		val proc	= builder.start()

		/* val in = */ spawn { spewLines(proc.getOutputStream, input) }
		val	err	= spawn { slurpLines(proc.getErrorStream) }
		val	out	= spawn { slurpLines(proc.getInputStream) }

		new External(proc, out, err)
	}

	private def slurpLines(st:InputStream):Seq[String] = {
		new InputStreamReader(st) use { _.readLines() }
	}

	private def spewLines(st:OutputStream, lines:Seq[String]):Unit = {
		new OutputStreamWriter(st) use { writer =>
			lines foreach { line =>
				writer write line
				writer write SystemProperties.line.separator
			}
		}
	}

	private val execute	= Execution.thread

	private def spawn[T](task: =>T):Thunk[T] =
		execute withResult thunk(task)
}

final class External(proc:Process, out:Thunk[Seq[String]], err:Thunk[Seq[String]]) {
	/** may throw exceptions when reading stdout and stderr of the process failed  */
	def result(destroy:Boolean):ExternalResult	= {
		if (destroy) {
			proc.destroy()
		}
		// avoid memory leak, see http://developer.java.sun.com/developer/qow/archive/68/
		proc.waitFor()
		ExternalResult(proc.exitValue, out(), err())
	}
}
