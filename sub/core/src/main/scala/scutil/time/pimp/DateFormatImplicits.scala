package scutil.time.pimp

import java.text._
import java.util.Date

import scutil.lang._

object DateFormatImplicits extends DateFormatImplicits

trait DateFormatImplicits {
    implicit def toDateFormatExt(peer:DateFormat)	= new DateFormatExt(peer)
}

final class DateFormatExt(peer:DateFormat) {
	def asMarshaller:Marshaller[Date,String]	=
			Marshaller(
				it => cloned format it, 
				it => 
						Catch.byType[ParseException] 
						.in {
							val	clone	= cloned
							clone setLenient false
							cloned parse it 
						}
						.toOption
			)
			
	// cloning should be generalized, but @see https://issues.scala-lang.org/browse/SI-3197
	
	/** DateFormat is not threadsafe, cloning helps */
	def cloned:DateFormat	= 
			peer.clone.asInstanceOf[DateFormat]
}
