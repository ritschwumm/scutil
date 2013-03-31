package scutil.log

import scutil.SourceLocation

case class LogEvent(level:LogLevel, location:SourceLocation, elements:Seq[Any])
