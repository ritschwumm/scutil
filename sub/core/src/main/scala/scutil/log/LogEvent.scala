package scutil.log

import scutil.lang.ISeq
import scutil.lang.SourceLocation
import scutil.time.MilliInstant

final case class LogEvent(
	level:LogLevel,
	values:ISeq[LogValue],
	timestamp:MilliInstant,
	location:SourceLocation
)
