package scutil.jtime.pimp

import java.text._
import java.util.Date

import scutil.lang._

object DateFormatImplicits extends DateFormatImplicits

trait DateFormatImplicits {
	implicit final class DateFormatExt(peer:DateFormat) {
		def toPrism:Prism[String,Date]	=
				Prism(
					it =>
							Catch.byType[ParseException]
							.in {
								val	clone	= cloned
								clone setLenient false
								clone parse it
							}
							.toOption,
					it => cloned format it
				)

		// cloning should be generalized, but @see https://issues.scala-lang.org/browse/SI-3197

		/** DateFormat is not threadsafe, cloning helps */
		@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
		def cloned:DateFormat	=
				peer.clone.asInstanceOf[DateFormat]
	}
}
