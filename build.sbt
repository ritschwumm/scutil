organization	:= "de.djini"
name			:= "scutil"
version			:= "0.60.0"

organization	in ThisBuild	:= organization.value
version			in ThisBuild	:= version.value

scalaVersion	in ThisBuild	:= "2.11.4"
scalacOptions	in ThisBuild	++= Seq(
	"-Ywarn-unused-import",
	"-Xfatal-warnings"
)

conflictManager	in ThisBuild	:= ConflictManager.strict
resolvers		in ThisBuild	+= "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
dependencyOverrides	in ThisBuild	++= Set(
	"org.scala-lang"	% "scala-library"	% (scalaVersion in ThisBuild).value,
	"org.scala-lang"	% "scala-reflect"	% (scalaVersion in ThisBuild).value
)

//------------------------------------------------------------------------------


lazy val `scutil`	=
		project 
		.in			(file("."))
		.aggregate	(`scutil-core`, `scutil-swing`, `scutil-extra`)
		.settings	(publishArtifact := false)

lazy val `scutil-core`	= 
		project 
		.in			(file("sub/core"))
		
lazy val `scutil-swing`	= 
		project 
		.in			(file("sub/swing"))
		.dependsOn	(`scutil-core`)
		
lazy val `scutil-extra`	= 
		project 
		.in			(file("sub/extra"))
		.dependsOn	(`scutil-core`)
