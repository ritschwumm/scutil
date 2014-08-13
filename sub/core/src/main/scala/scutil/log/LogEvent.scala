package scutil.log

import scutil.lang.ISeq
import scutil.lang.SourceLocation
import scutil.time.MilliInstant

final case class LogEvent(level:LogLevel, elements:ISeq[Any], timestamp:MilliInstant, location:SourceLocation)
