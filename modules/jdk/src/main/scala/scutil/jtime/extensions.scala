package scutil.jtime

import scutil.jtime.extension._

object extensions extends extensions
trait extensions
	extends	CalendarImplicits
	with	DateImplicits
	with	DateFormatImplicits
	with	InstantImplicits
	with	MilliInstantImplicits
