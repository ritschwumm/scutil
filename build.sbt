inThisBuild(Seq(
	organization	:= "de.djini",
	version			:= "0.85.0",
	
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
	resolvers		+= "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
))

lazy val warts	=
		Seq(
			Wart.Any2StringAdd,
			Wart.EitherProjectionPartial,
			Wart.OptionPartial,
			Wart.Enumeration,
			Wart.FinalCaseClass,
			Wart.JavaConversions,
			Wart.Option2Iterable,
			Wart.TryPartial
		)
		
//------------------------------------------------------------------------------

lazy val `scutil`	=
		(project	in	file("."))
		.aggregate	(`scutil-core`, `scutil-swing`, `scutil-xml`, `scutil-uid`)
		.settings	(publishArtifact := false)

lazy val `scutil-core`	=
		(project	in	file("sub/core"))
		.settings(
			scalacOptions	++= Seq(
				// "-Ymacro-debug-lite",
				"-language:implicitConversions",
				// "-language:existentials",
				"-language:higherKinds"//,
				// "-language:reflectiveCalls",
				// "-language:dynamics",
				// "-language:postfixOps",
				// "-language:experimental.macros",
			),
			
			(sourceGenerators in Compile)	<+= (sourceManaged in Compile) map Boilerplate.generate,
			
			libraryDependencies	++= Seq(
				"org.scala-lang"	%	"scala-reflect"	% scalaVersion.value	% "compile",
				"org.specs2"		%%	"specs2-core"	% "3.8.4"				% "test"
			),
			
			wartremoverErrors ++= warts,
			
			//------------------------------------------------------------------------------
			
			scalacOptions in (Compile, console) := (scalacOptions in (Compile, console)).value filterNot { it =>
				it == "-Ywarn-unused-import" ||
				it == "-Xfatal-warnings"
			},
			initialCommands in console	:= """
				import scala.language.postfixOps
				import java.io.File
				import scutil.lang._
				import scutil.implicits._
				import scutil.io.Files._
				import scutil.io.Charsets.{ iso_8859_1, utf_8 }
				import scutil.io.Base64
				import scutil.io.URIComponent
				import scutil.text.Human
				import scutil.time._
				import scutil.math._
				import scutil.platform._
			"""
		)

lazy val `scutil-swing`	=
		(project	in	file("sub/swing"))
		.settings(
			scalacOptions	++= Seq(
				"-language:implicitConversions"//,
				// "-language:existentials",
				// "-language:higherKinds",
				// "-language:reflectiveCalls",
				// "-language:dynamics",
				// "-language:postfixOps",
				// "-language:experimental.macros",
			),
			
			wartremoverErrors ++= warts
		)
		.dependsOn	(`scutil-core`)
		
lazy val `scutil-xml`	=
		(project	in	file("sub/xml"))
		.settings(
			scalacOptions	++= Seq(
				"-language:implicitConversions"//,
				// "-language:existentials",
				// "-language:higherKinds",
				// "-language:reflectiveCalls",
				// "-language:dynamics",
				// "-language:postfixOps",
				// "-language:experimental.macros",
			),
			
			libraryDependencies	++= Seq(
				"org.scala-lang.modules"	%% "scala-xml"	% "1.0.5"	% "compile"
			),
			
			wartremoverErrors ++= warts
		)
		.dependsOn	(`scutil-core`)
		
lazy val `scutil-uid`	=
		(project	in	file("sub/uid"))
		.settings(
			scalacOptions	++= Seq(
				//"-language:implicitConversions",
				// "-language:existentials",
				// "-language:higherKinds",
				// "-language:reflectiveCalls",
				// "-language:dynamics",
				// "-language:postfixOps",
				// "-language:experimental.macros",
			),
			
			wartremoverErrors ++= warts
		)
		.dependsOn	(`scutil-core`)
