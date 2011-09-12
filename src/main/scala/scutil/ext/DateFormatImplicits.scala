package scutil.ext

import java.text._
import java.util.Date

import scala.util.control.Exception._

import scutil.Marshaller

object DateFormatImplicits extends DateFormatImplicits

trait DateFormatImplicits {
    implicit def toDateFormatExt(delegate:DateFormat)	= new DateFormatExt(delegate)
}

final class DateFormatExt(delegate:DateFormat) {
	def asMarshaller:Marshaller[Date,String]	= Marshaller(
			it => cloned format it, 
			it => catching(classOf[ParseException]) opt { cloned parse it })
			
	// NOTE DateFormat is not threadsafe
	def cloned:DateFormat	= {
		val clone	= delegate.clone.asInstanceOf[DateFormat]
		clone setLenient false
		clone
	}
}
