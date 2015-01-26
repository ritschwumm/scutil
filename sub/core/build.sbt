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
	"org.specs2"		%%	"specs2"		% "2.4.15"				% "test"
)
