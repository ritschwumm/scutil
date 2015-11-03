name	:= "scutil-core"

scalacOptions	++= Seq(
	// "-Ymacro-debug-lite",
	"-deprecation",
	"-unchecked",
	"-language:implicitConversions",
	// "-language:existentials",
	"-language:higherKinds",
	// "-language:reflectiveCalls",
	// "-language:dynamics",
	// "-language:postfixOps",
	// "-language:experimental.macros",
	"-feature",
	"-optimize"
)

(sourceGenerators in Compile)	<+= (sourceManaged in Compile) map Boilerplate.generate

libraryDependencies	++= Seq(
	"org.scala-lang"	%	"scala-reflect"	% scalaVersion.value	% "compile",
	"org.specs2"		%%	"specs2-core"	% "3.6.4"				% "test"
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
	import scutil.io.Charsets.utf_8
	import scutil.io.Base64
	import scutil.io.URIComponent
	import scutil.text.Human
	import scutil.time._
	import scutil.math._
"""
