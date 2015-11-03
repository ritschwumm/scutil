inThisBuild(Seq(
	organization	:= "de.djini",
	version			:= "0.72.0",
	
	scalaVersion	:= "2.11.7",
	scalacOptions	++= Seq(
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
