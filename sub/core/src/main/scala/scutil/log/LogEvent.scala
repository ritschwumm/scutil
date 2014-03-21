package scutil.log

import scutil.lang.SourceLocation
import scutil.time.MilliInstant

final case class LogEvent(level:LogLevel, elements:Seq[Any], timestamp:MilliInstant, location:SourceLocation)
