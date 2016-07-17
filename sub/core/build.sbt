name	:= "scutil-core"

scalacOptions	++= Seq(
	// "-Ymacro-debug-lite",
	"-language:implicitConversions",
	// "-language:existentials",
	"-language:higherKinds"//,
	// "-language:reflectiveCalls",
	// "-language:dynamics",
	// "-language:postfixOps",
	// "-language:experimental.macros",
)

(sourceGenerators in Compile)	<+= (sourceManaged in Compile) map Boilerplate.generate

libraryDependencies	++= Seq(
	"org.scala-lang"	%	"scala-reflect"	% scalaVersion.value	% "compile",
	"org.specs2"		%%	"specs2-core"	% "3.8.4"				% "test"
)

wartremoverErrors ++= Seq(
	Wart.Any2StringAdd,
	Wart.EitherProjectionPartial,
	Wart.OptionPartial,
	Wart.Enumeration,
	Wart.FinalCaseClass,
	Wart.JavaConversions,
	Wart.Option2Iterable,
	Wart.TryPartial
)

//------------------------------------------------------------------------------

scalacOptions in (Compile, console) := (scalacOptions in (Compile, console)).value filterNot { it =>
	it == "-Ywarn-unused-import" ||
	it == "-Xfatal-warnings"
}
initialCommands in console	:= """
	import scala.language.postfixOps
	import java.io.File
	import scutil.lang._
	import scutil.implicits._
	import scutil.io.Files._
	import scutil.io.Charsets.{ iso_8859_1, utf_8 }
	import scutil.io.Base64
	import scutil.io.URIComponent
	import scutil.text.Human
	import scutil.time._
	import scutil.math._
	import scutil.platform._
"""
