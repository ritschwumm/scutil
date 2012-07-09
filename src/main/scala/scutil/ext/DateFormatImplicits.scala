package scutil.ext

import java.text._
import java.util.Date

import scala.util.control.Exception._

import scutil.data.Marshaller

object DateFormatImplicits extends DateFormatImplicits

trait DateFormatImplicits {
    implicit def toDateFormatExt(delegate:DateFormat)	= new DateFormatExt(delegate)
}

final class DateFormatExt(delegate:DateFormat) {
	def asMarshaller:Marshaller[Date,String]	= Marshaller(
			it => cloned format it, 
			it => catching(classOf[ParseException]) opt {
				val	clone	= cloned
				clone setLenient false
				cloned parse it 
			})
			
	// cloning should be generalized, but @see https://issues.scala-lang.org/browse/SI-3197
	
	/** DateFormat is not threadsafe, cloning helps */
	def cloned:DateFormat	= 
			delegate.clone.asInstanceOf[DateFormat]
}
