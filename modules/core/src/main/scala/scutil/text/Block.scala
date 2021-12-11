package scutil.text

import scutil.core.implicits.*

object Block {
	def generate(parts:Seq[String], args:Seq[String]):String	= {
		// check invariants
		require(parts.size >= 1,				"parts must have at least one element")
		require(args.size == parts.size - 1,	"args must have exactly one element less than parts")

		val cleanParts:Seq[String]	= removeSyntax(parts)

		// head is legal here, we always have a first part
		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		val initialArgIndent	= indentOf(cleanParts.head)

		val argIndentOpts	=
			cleanParts map { part =>
				part lastIndexOfChar '\n' map { idx =>
					indentOf(part substring (idx+1))
				}
			}

		// tail is legal here, scanLeft always returns at least one element
		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		val argIndents	=
			argIndentOpts.scanLeft(initialArgIndent){ (old, cur) => cur getOrElse old }.tail

		// last is legal here, we always have a clean part
		@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
		val indented:String	=
			(cleanParts lazyZip args lazyZip argIndents)
			.map { (part, arg, argIndent) =>
				val indentedArg	= unixLf(arg).replace("\n", "\n" + argIndent)
				part + indentedArg
			}
			.appended(cleanParts.last)
			.mkString

		unindent(indented)
	}

	private def removeSyntax(parts:Seq[String]):Seq[String]	=
		parts.zipWithIndex map { case (rawPart, index) =>
			val first	= index == 0
			val last	= index == parts.lastIndex

			val unixPart	= unixLf(rawPart)

			val front	=
				first
				.flatOption (unixPart indexOfChar		'\n')
				.filter		{ idx => allBlank(unixPart.substring(0, idx)) }
				.map		(_ + 1)
				.getOrElse	(0)

			val behind	=
				last
				.flatOption	(unixPart lastIndexOfChar	'\n')
				.filter		{ idx => allBlank(unixPart.substring(idx+1, unixPart.length)) }
				.getOrElse	(unixPart.length)

			if		(front == 0 && behind == unixPart.length)	unixPart
			else if	(front > behind)							""
			else												unixPart.substring(front, behind)
		}

	private def unindent(indented:String):String	= {
		val indentedLines:Seq[String]	= indented splitAroundChar '\n'

		val relevantIndents:Seq[String]	=
			indentedLines filter (_.nonEmpty) map indentOf

		if (relevantIndents.nonEmpty) {
			// reduce is legal here, we a relevantIndent
			@SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
			val commonIndentSize:Int	= relevantIndents.reduce(commonPrefix).length

			indentedLines
			.map { line =>
				if (line.length >= commonIndentSize)	line substring commonIndentSize
				else									""
			}
			.mkString ("\n")
		}
		else indented
	}

	private def commonPrefix(a:String, b:String):String	= {
		var	i	= 0
		while (i < a.length && i < b.length && a.charAt(i) == b.charAt(i))	i	+= 1
		a.substring(0, i)
	}

	//------------------------------------------------------------------------------

	private val blankChar:Char=>Boolean			= it => it == ' ' || it == '\t'
	private def allBlank(s:String):Boolean		= s forall blankChar
	private def indentOf(line:String):String	= line takeWhile blankChar
	private def unixLf(s:String):String			= s.replace("\r\n", "\n")
}
