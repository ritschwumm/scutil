name	:= "scutil-extra"

scalacOptions	++= Seq(
	// "-Ymacro-debug-lite",
	"-deprecation",
	"-unchecked",
	"-language:implicitConversions",
	// "-language:existentials",
	// "-language:higherKinds",
	// "-language:reflectiveCalls",
	// "-language:dynamics",
	// "-language:postfixOps",
	// "-language:experimental.macros",
	"-feature",
	"-optimize"
)

libraryDependencies	++= Seq(
	"org.scala-lang.modules"	%% "scala-xml"	% "1.0.2"	% "compile"
)
