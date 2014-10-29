package scutil.text

object Text {
	/** indent every line with a single tab */
	def indent(prefix:String, str:String):String =
			str replaceAll ("(?m)^", prefix)
			
	/** treats all combinations of CR and LF as line endings */
	def expandTabs(width:Int, text:String):String = {
		val	out	= new StringBuilder
		var col	= 0
		var i	= 0
		while (i < text.length) {
			text charAt i match {
				case '\r'	=> 
					out append '\r'
					col	= 0
				case '\n'	=> 
					out append '\n'
					col	= 0
				case '\t'	=>
					do {
						out append ' '
						col	+= 1
					}
					while (col % width != 0)
				case x	=>
					out append x
					col	+= 1
			} 
			i	+= 1
		}
		out.toString
	}
	
	private val Strip	= """^\s*\|(.*)$""".r
	
	/**
	similar to String's stripMargin method, this removes (optional) leading whitespace and a '|' character.
	differently from stripMargin it ignores lines where stripping cannot be applied.
	*/
	def stripMarginOnly(s:String):String	= 
			s.lines collect { case Strip(it) => it } mkString "\n"
}
