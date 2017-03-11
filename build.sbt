inThisBuild(Seq(
	organization	:= "de.djini",
	version			:= "0.94.0",
	
	scalaVersion	:= "2.12.1",
	scalacOptions	++= Seq(
		"-deprecation",
		"-unchecked",
		"-feature",
		"-opt:l:project",
		"-Ywarn-unused-import",
		"-Xfatal-warnings",
		"-Xlint"
	),
	scalaJSUseRhino	:= true,
	
	conflictManager	:= ConflictManager.strict,
	resolvers		+= "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
))

lazy val warts	=
		Seq(
			Wart.StringPlusAny,
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
		.aggregate	(
			`scutil-base-jvm`,
			`scutil-base-js`,
			`scutil-core`,
			`scutil-swing`,
			`scutil-xml`,
			`scutil-uid`
		)
		.settings	(publishArtifact := false)

lazy val `scutil-base`	=
		(crossProject crossType CrossType.Pure	in	file("sub/base"))
		.enablePlugins(
			spray.boilerplate.BoilerplatePlugin
		)
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
			// only here because -> leads to inliner warnings
			(scalacOptions in Compile) := (scalacOptions in Compile).value filterNot { _ == "-Xfatal-warnings" },
			libraryDependencies	++= Seq(
				"org.scala-lang"	%	"scala-reflect"	% scalaVersion.value	% "provided",
				"org.specs2"		%%	"specs2-core"	% "3.8.6"				% "test"
			),
			boilerplateSource in Compile := baseDirectory.value.getParentFile / "src" / "main" / "boilerplate",
			wartremoverErrors ++= warts,
			
			//------------------------------------------------------------------------------
			
			scalacOptions in (Compile, console) := (scalacOptions in (Compile, console)).value filterNot { it =>
				it == "-Ywarn-unused-import" ||
				it == "-Xfatal-warnings"
			}
		)
		.jvmSettings()
		.jsSettings()
lazy val `scutil-base-jvm`	= `scutil-base`.jvm
lazy val `scutil-base-js`	= `scutil-base`.js

lazy val `scutil-core`	=
		(project	in	file("sub/core"))
		.settings(
			scalacOptions	++= Seq(
				// "-Ymacro-debug-lite",
				"-language:implicitConversions"//,
				// "-language:existentials",
				// "-language:higherKinds",
				// "-language:reflectiveCalls",
				// "-language:dynamics",
				// "-language:postfixOps",
				// "-language:experimental.macros",
			),
			
			libraryDependencies	++= Seq(
				"org.scala-lang"	%	"scala-reflect"	% scalaVersion.value	% "provided",
				"org.specs2"		%%	"specs2-core"	% "3.8.6"				% "test"
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
				import scutil.base.implicits._
				import scutil.core.implicits._
				import scutil.io.Files._
				import scutil.codec.Charsets.{ iso_8859_1, utf_8 }
				import scutil.codec.Base64
				import scutil.codec.URIComponent
				import scutil.text.Human
				import scutil.time._
				import scutil.math._
				import scutil.number._
				import scutil.platform._
			"""
		)
		.dependsOn	(`scutil-base-jvm`)

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
				"org.scala-lang.modules"	%% "scala-xml"	% "1.0.6"	% "compile"
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
