package scutil.log

enum LogAtom {
	case LogString(value:String)
	case LogThrowable(value:Throwable)
}
