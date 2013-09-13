package scutil.log

import scutil.lang.SourceLocation

case class LogEvent(level:LogLevel, location:SourceLocation, elements:Seq[Any])
