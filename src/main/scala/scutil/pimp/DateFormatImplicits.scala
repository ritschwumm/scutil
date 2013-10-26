package scutil.pimp

import java.text._
import java.util.Date

import scala.util.control.Exception._

import scutil.lang._

object DateFormatImplicits extends DateFormatImplicits

trait DateFormatImplicits {
    implicit def toDateFormatExt(peer:DateFormat)	= new DateFormatExt(peer)
}

final class DateFormatExt(peer:DateFormat) {
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
			peer.clone.asInstanceOf[DateFormat]
}
