name	:= "scutil-core"

libraryDependencies	++= Seq(
	"org.scala-lang"	%	"scala-reflect"	% scalaVersion.value	% "compile",
	"org.specs2"		%%	"specs2"		% "2.3.13"				% "test"	exclude("org.scala-lang", "scala-library") exclude("org.scala-lang", "scala-reflect")
)

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
