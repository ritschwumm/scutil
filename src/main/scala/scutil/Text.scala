package scutil

object Text {
	/*
	private val Strip	= new scala.util.matching.Regex("""^\s*\|\t(.*)$""")
	def strip(s:String):String	= s.lines collect { case Strip(it) => it } mkString "\n"
	*/
	
	/** indent every line with a single tab */
	def indent(prefix:String, str:String):String =
			str.replaceAll("(?m)^", prefix)
			
	/** expects unix line endings */
	def expandTabs(width:Int, str:String):String = {
		val	out	= new StringBuilder()
		var col	= 0
		str foreach { _ match {
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
		} }
		out.toString
	}
}
