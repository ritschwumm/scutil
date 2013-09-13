name			:= "scutil"

organization	:= "de.djini"

version			:= "0.25.0"

scalaVersion	:= "2.10.2"

// crossScalaVersions	:= Seq("2.9.2", "2.10.0")

libraryDependencies	++= Seq(
	"org.scala-lang"	%	"scala-reflect"	% scalaVersion.value	% "compile",
	"org.specs2"		%%	"specs2"		% "1.14"				% "test"		exclude("org.scala-lang", "scala-library")
)

scalacOptions	++= Seq(
	// "-Ymacro-debug-lite",
	"-deprecation",
	"-unchecked",
	"-language:implicitConversions",
	"-language:existentials",
	"-language:higherKinds",
	"-language:reflectiveCalls",
	// "-language:dynamics",
	// "-language:postfixOps",
	// "-language:experimental.macros",
	"-feature"
)

(sourceGenerators in Compile)	<+= (sourceManaged in Compile) map Boilerplate.generate
