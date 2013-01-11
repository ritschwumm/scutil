name			:= "scutil"

organization	:= "de.djini"

version			:= "0.15.0"

scalaVersion	:= "2.10.0"

// crossScalaVersions	:= Seq("2.9.2", "2.10.0")

libraryDependencies	++= Seq(
	"org.specs2"	%% "specs2"	% "1.13"	% "test"
)

scalacOptions	++= Seq(
	"-deprecation",
	"-unchecked",
	"-language:implicitConversions",
	"-language:existentials",
	"-language:higherKinds",
	"-language:reflectiveCalls",
	// "-language:dynamics",
	// "-language:postfixOps",
	// "-language:experimental.macros"
	"-feature"
)
