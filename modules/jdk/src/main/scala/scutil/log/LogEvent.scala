package scutil.log

import scutil.lang.SourceLocation
import scutil.time.MilliInstant

final case class LogEvent(
	level:LogLevel,
	values:Seq[LogValue],
	timestamp:MilliInstant,
	location:SourceLocation
)
