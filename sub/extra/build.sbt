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
