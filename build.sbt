name			:= "scutil"

organization	:= "de.djini"

version			:= "0.9.0"

scalaVersion	:= "2.9.2"

libraryDependencies	++= Seq(
	"org.specs2"	%% "specs2"	% "1.11"	% "test"
)

scalacOptions	++= Seq("-deprecation", "-unchecked")
