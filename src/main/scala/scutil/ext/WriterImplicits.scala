package scutil.ext

import java.io._

object WriterImplicits extends WriterImplicits

trait WriterImplicits {
    implicit def toWriterExt(delegate:Writer)	= new WriterExt(delegate)
}

/** utility methods for Writer objects */ 
final class WriterExt(delegate:Writer) {
	val	lineSep	= System getProperty "line.separator"
	
	/** write a sequence of lines into this File */
	def writeLines(lines:Seq[String]):Unit = 
			lines foreach { line =>
				delegate write line
				delegate write lineSep
			}
}
