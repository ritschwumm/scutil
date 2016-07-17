inThisBuild(Seq(
	organization	:= "de.djini",
	version			:= "0.83.0",
	
	scalaVersion	:= "2.11.8",
	scalacOptions	++= Seq(
		"-deprecation",
		"-unchecked",
		"-feature",
		"-optimize",
		"-Ywarn-unused-import",
		"-Xfatal-warnings"
	),
	
	conflictManager	:= ConflictManager.strict,
	resolvers		+= "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
	dependencyOverrides	++= Set(
		"org.scala-lang"	% "scala-library"	% scalaVersion.value,
		"org.scala-lang"	% "scala-reflect"	% scalaVersion.value
	)
))

//------------------------------------------------------------------------------

lazy val `scutil`	=
		project
		.in			(file("."))
		.aggregate	(`scutil-core`, `scutil-swing`, `scutil-xml`, `scutil-uid`)
		.settings	(publishArtifact := false)

lazy val `scutil-core`	=
		project
		.in			(file("sub/core"))
		
lazy val `scutil-swing`	=
		project
		.in			(file("sub/swing"))
		.dependsOn	(`scutil-core`)
		
lazy val `scutil-xml`	=
		project
		.in			(file("sub/xml"))
		.dependsOn	(`scutil-core`)
		
lazy val `scutil-uid`	=
		project
		.in			(file("sub/uid"))
		.dependsOn	(`scutil-core`)
