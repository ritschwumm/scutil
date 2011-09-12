name			:= "scutil"

organization	:= "de.djini"

version			:= "0.0.4"

scalaVersion	:= "2.9.0-1"

//publishArtifact in (Compile, packageBin)	:= false

publishArtifact in (Compile, packageDoc)	:= false

publishArtifact in (Compile, packageSrc)	:= false

libraryDependencies	++= Seq(
	"org.scala-tools.testing"	%% "specs"	% "1.6.8"	% "test"
)

scalacOptions	++= Seq("-deprecation", "-unchecked")
