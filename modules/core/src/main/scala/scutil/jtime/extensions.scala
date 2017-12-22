package scutil.jtime

import scutil.jtime.pimp._

object extensions extends extensions
trait extensions
		extends	DateImplicits
		with	DateFormatImplicits
		with	CalendarImplicits
		with	MilliInstantImplicits
		with	MilliDurationImplicits
