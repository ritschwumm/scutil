import org.scalajs.sbtplugin.cross.CrossProject
import spray.boilerplate.BoilerplatePlugin

inThisBuild(Seq(
	organization	:= "de.djini",
	version			:= "0.109.0",
	
	scalaVersion	:= "2.12.2",
	scalacOptions	++= Seq(
		"-deprecation",
		"-unchecked",
		"-feature",
		"-opt:l:project",
		"-Xfatal-warnings",
		"-Xlint",
		"-Ypartial-unification"
	),
	
	conflictManager	:= ConflictManager.strict,
	resolvers		+= "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
	resolvers 		+= Resolver sonatypeRepo "releases",
	addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
))

lazy val fixConsoleSettings	=
		Seq(
			scalacOptions in (Compile, console) ~= (opts => opts filterNot Set(
				"-Xlint",
				"-Xfatal-warnings"
			))
		)
		
lazy val noTestSettings	=
		Seq(
			test		:= {},
			testQuick	:= {}
		)

lazy val wartRemoverSetting	=
		wartremoverErrors	++= Seq(
			Wart.StringPlusAny,
			Wart.EitherProjectionPartial,
			Wart.OptionPartial,
			Wart.Enumeration,
			Wart.FinalCaseClass,
			Wart.JavaConversions,
			Wart.Option2Iterable,
			Wart.TryPartial
		)
		
// (crossProject crossType CrossType.Pure in base)
def myCrossProject(id:String, base:File):CrossProject	=
		CrossProject(id + "-jvm", id + "-js", base, CrossType.Pure).settings(name := id)
	
lazy val `scutil`	=
		(project	in	file("."))
		.aggregate(
			`scutil-base-jvm`,
			`scutil-base-js`,
			`scutil-core`,
			`scutil-swing`,
			`scutil-xml`,
			`scutil-uid`
		)
		.settings(
			publishArtifact	:= false
		)
		
//------------------------------------------------------------------------------

lazy val `scutil-base`	=
		myCrossProject("scutil-base", file("sub/base"))
		.enablePlugins(
			BoilerplatePlugin
		)
		.settings(
			fixConsoleSettings,
			wartRemoverSetting,
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
			libraryDependencies	++= Seq(
				"org.scala-lang"	%	"scala-reflect"	% scalaVersion.value	% "provided",
				"org.specs2"		%%	"specs2-core"	% "3.8.9"				% "test"
			),
			boilerplateSource in Compile := baseDirectory.value.getParentFile / "src" / "main" / "boilerplate"
		)
		.jvmSettings()
		.jsSettings(
			noTestSettings
		)
lazy val `scutil-base-jvm`	= `scutil-base`.jvm
lazy val `scutil-base-js`	= `scutil-base`.js

lazy val `scutil-core`	=
		(project	in	file("sub/core"))
		.settings(
			fixConsoleSettings,
			wartRemoverSetting,
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
				"org.specs2"		%%	"specs2-core"	% "3.8.9"				% "test"
			),
			
			//------------------------------------------------------------------------------
			
			initialCommands in console	:= """
				import scala.language.postfixOps
				import java.io.File
				import scutil.lang._
				import scutil.lang.Charsets.{ us_ascii, iso_8859_1, utf_8 }
				import scutil.base.implicits._
				import scutil.core.implicits._
				import scutil.codec.Base64
				import scutil.codec.URIComponent
				import scutil.text.Human
				import scutil.time._
				import scutil.math._
				import scutil.number._
				import scutil.platform._
				import scutil.platform.Platform._
			"""
		)
		.dependsOn	(`scutil-base-jvm`)

lazy val `scutil-swing`	=
		(project	in	file("sub/swing"))
		.settings(
			fixConsoleSettings,
			wartRemoverSetting,
			scalacOptions	++= Seq(
				"-language:implicitConversions"//,
				// "-language:existentials",
				// "-language:higherKinds",
				// "-language:reflectiveCalls",
				// "-language:dynamics",
				// "-language:postfixOps",
				// "-language:experimental.macros",
			)
		)
		.dependsOn(
			`scutil-core`
		)
		
lazy val `scutil-xml`	=
		(project	in	file("sub/xml"))
		.settings(
			fixConsoleSettings,
			wartRemoverSetting,
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
			)
		)
		.dependsOn(
			`scutil-core`
		)
		
lazy val `scutil-uid`	=
		(project	in	file("sub/uid"))
		.settings(
			fixConsoleSettings,
			wartRemoverSetting,
			scalacOptions	++= Seq(
				//"-language:implicitConversions",
				// "-language:existentials",
				// "-language:higherKinds",
				// "-language:reflectiveCalls",
				// "-language:dynamics",
				// "-language:postfixOps",
				// "-language:experimental.macros",
			)
		)
		.dependsOn(
			`scutil-core`
		)
