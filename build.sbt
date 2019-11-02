import spray.boilerplate.BoilerplatePlugin
import sbtcrossproject.{ CrossProject, CrossType, Platform }

inThisBuild(Seq(
	organization	:= "de.djini",
	version			:= "0.162.0",

	scalaVersion	:= "2.13.1",
	scalacOptions	++= Seq(
		"-deprecation",
		"-unchecked",
		"-feature",
		"-opt:l:method",
		"-opt:l:inline",
		"-opt-inline-from:scutil.**",
		"-Xfatal-warnings",
		"-Xlint",
	),

	conflictManager	:= ConflictManager.strict withOrganization "^(?!(org\\.scala-lang|org\\.scala-js)(\\..*)?)$",
	resolvers 		+= Resolver sonatypeRepo "releases",
	addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),

	wartremoverErrors	++= Seq(
		Wart.AsInstanceOf,
		Wart.IsInstanceOf,
		Wart.StringPlusAny,
		//Wart.ToString,
		Wart.EitherProjectionPartial,
		Wart.OptionPartial,
		Wart.TryPartial,
		Wart.Enumeration,
		Wart.FinalCaseClass,
		Wart.JavaConversions,
		Wart.Option2Iterable,
		Wart.JavaSerializable,
		//Wart.Any,
		Wart.AnyVal,
		//Wart.Nothing,
		Wart.ArrayEquals,
		Wart.ImplicitParameter,
		Wart.ExplicitImplicitTypes,
		Wart.LeakingSealed,
		//Wart.DefaultArguments,
		//Wart.Overloading,
		//Wart.PublicInference,
		Wart.TraversableOps,
	)
))

lazy val fixConsoleSettings	=
		Seq(
			Compile / console / scalacOptions ~= (opts => opts filterNot Set(
				"-Xlint",
				"-Xfatal-warnings"
			))
		)

lazy val noTestSettings	=
		Seq(
			test		:= {},
			testQuick	:= {}
		)

// (crossProject crossType CrossType.Pure in base)
def myCrossProject(id:String, base:File, crossType:CrossType):CrossProject	=
		CrossProject(
			id		= id,
			base	= base,
		)(
			JVMPlatform,
			JSPlatform
		)
		.crossType(crossType)
		.settings(
			name := id
		)
		.configurePlatform(JVMPlatform)	(_ withId (id + "-jvm"))
		.configurePlatform(JSPlatform)	(_ withId (id + "-js"))

lazy val `scutil`	=
		(project	in	file("."))
		.aggregate(
			`scutil-base-jvm`,
			`scutil-base-js`,
			`scutil-core`,
			`scutil-swing`,
			`scutil-xml`,
			`scutil-guid-jvm`,
			`scutil-guid-js`
		)
		.settings(
			publishArtifact	:= false
		)

//------------------------------------------------------------------------------

lazy val `scutil-base`	=
		myCrossProject("scutil-base", file("modules/base"), CrossType.Pure)
		.enablePlugins(
			BoilerplatePlugin
		)
		.settings(
			fixConsoleSettings,
			scalacOptions	++= Seq(
				// "-language:implicitConversions",
				// "-language:existentials",
				// "-language:reflectiveCalls",
				// "-language:dynamics",
				// "-language:experimental.macros",
				// "-Ymacro-debug-lite",
			),
			libraryDependencies	++= Seq(
				"org.scala-lang"	%	"scala-reflect"	% scalaVersion.value	% "provided",
				"org.specs2"		%%	"specs2-core"	% "4.8.0"				% "test"
			),
			Compile / boilerplateSource	:= baseDirectory.value.getParentFile / "src" / "main" / "boilerplate"
		)
		.jvmSettings()
		.jsSettings(
			noTestSettings
		)
lazy val `scutil-base-jvm`	= `scutil-base`.jvm
lazy val `scutil-base-js`	= `scutil-base`.js

lazy val `scutil-core`	=
		(project	in	file("modules/core"))
		.settings(
			fixConsoleSettings,
			scalacOptions	++= Seq(
				"-language:implicitConversions"
				// "-language:existentials",
				// "-language:reflectiveCalls",
				// "-language:dynamics",
				// "-language:experimental.macros",
				// "-Ymacro-debug-lite",
			),
			libraryDependencies	++= Seq(
				"org.scala-lang"	%	"scala-reflect"	% scalaVersion.value	% "provided",
				"org.specs2"		%%	"specs2-core"	% "4.8.0"				% "test"
			),

			//------------------------------------------------------------------------------

			console / initialCommands	:= """
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
		(project	in	file("modules/swing"))
		.settings(
			fixConsoleSettings,
			scalacOptions	++= Seq(
				"-language:implicitConversions"
				// "-language:existentials",
				// "-language:reflectiveCalls",
				// "-language:dynamics",
				// "-language:experimental.macros",
			)
		)
		.dependsOn(
			`scutil-core`
		)

lazy val `scutil-xml`	=
		(project	in	file("modules/xml"))
		.settings(
			fixConsoleSettings,
			scalacOptions	++= Seq(
				// "-language:implicitConversions",
				// "-language:existentials",
				// "-language:reflectiveCalls",
				// "-language:dynamics",
				// "-language:experimental.macros",
			),
			libraryDependencies	++= Seq(
				"org.scala-lang.modules"	%% "scala-xml"	% "1.2.0"	% "compile"
			)
		)
		.dependsOn(
			`scutil-core`
		)

lazy val `scutil-guid`	=
		myCrossProject("scutil-guid", file("modules/guid"), CrossType.Full)
		.settings(
			fixConsoleSettings,
			scalacOptions	++= Seq(
				//"-language:implicitConversions",
				// "-language:existentials",
				// "-language:reflectiveCalls",
				// "-language:dynamics",
				// "-language:experimental.macros",
			)
		)
		.dependsOn(
			`scutil-base`
		)
		.jvmSettings()
		.jsSettings(
			noTestSettings,
			libraryDependencies	++= Seq(
				"org.scala-js"	%%%	"scalajs-dom"	% "0.9.7"	% "compile"
			)
		)
lazy val `scutil-guid-jvm`	= `scutil-guid`.jvm
lazy val `scutil-guid-js`	= `scutil-guid`.js
