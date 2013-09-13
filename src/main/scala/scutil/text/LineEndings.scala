package scutil.text

import scutil.platform.SystemProperties

object LineEndings {
	val platform	= SystemProperties.line.separator
	
	val CR		= "\r"
	val LF		= "\n"
	val CRLF	= "\r\n"
	
	val	unix	= LF
	val windows	= CRLF
	val mac		= CR
}
