package scutil.text

object Text {
	/** indent every line with a single tab */
	def indent(prefix:String, str:String):String =
		str.replaceAll("(?m)^", prefix)

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
		s.linesIterator collect { case Strip(it) => it } mkString "\n"

	def table(rows:Seq[Seq[String]]):Seq[String]	= {
		val widths	=
				(rows foldLeft Vector.empty[Int]) { (widths, row) =>
					widths.zipAll(row map (_.length), 0, 0) map { case (a,b) => a max b }
				}

		val lines	= rows map { cells =>
			cells
			.zipAll (widths, "", 0)
			.map { case (cell, width) =>
				cell + " " * ((width - cell.length max 0))
			}
			.mkString ("│", "│", "│")
		}

		val topRuler	= widths .map { width => "─" * width } .mkString ("┌", "┬", "┐")
		val midRuler	= widths .map { width => "─" * width } .mkString ("├", "┼", "┤")
		val bottomRuler	= widths .map { width => "─" * width } .mkString ("└", "┴", "┘")

		val spersed	=
			if (lines.isEmpty)	Vector.empty[String]
			else				lines flatMap { it => Vector(it, midRuler) } dropRight 1

		Vector(topRuler) ++ spersed ++ Vector(bottomRuler)
	}
}
