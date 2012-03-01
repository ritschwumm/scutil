package scutil.log

import LogLevels._

case class LogEntry(source:Class[_], trace:Option[StackTraceElement], level:LogLevel, elements:Seq[Any])
