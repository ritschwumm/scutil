name			:= "scutil"

organization	:= "de.djini"

version			:= "0.18.0"

scalaVersion	:= "2.10.1"

// crossScalaVersions	:= Seq("2.9.2", "2.10.0")

libraryDependencies	++= Seq(
	"org.specs2"	%% "specs2"	% "1.14"	% "test"
)

libraryDependencies	<+= (scalaVersion) { "org.scala-lang" % "scala-reflect" % _ }

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
