import spray.boilerplate.BoilerplatePlugin
import sbtcrossproject.{ CrossProject, CrossType, Platform }

inThisBuild(Seq(
	organization	:= "de.djini",
	version			:= "0.149.0",

	scalaVersion	:= "2.12.7",
	scalacOptions	++= Seq(
		"-deprecation",
		"-unchecked",
		"-feature",
		"-opt:l:inline",
		"-opt-inline-from:scutil.**",
		"-Xfatal-warnings",
		"-Xlint",
		"-Ypartial-unification"
	),

	conflictManager	:= ConflictManager.strict,
	resolvers		+= "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
	resolvers 		+= Resolver sonatypeRepo "releases",
	addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8"),

	wartremoverErrors	++= Seq(
		Wart.AsInstanceOf,
		Wart.IsInstanceOf,
		Wart.StringPlusAny,
		Wart.EitherProjectionPartial,
		Wart.OptionPartial,
		Wart.Enumeration,
		Wart.FinalCaseClass,
		Wart.JavaConversions,
		Wart.Option2Iterable,
		Wart.TryPartial,
		Wart.JavaSerializable,
		//Wart.Any,
		Wart.AnyVal,
		//Wart.Nothing,
		Wart.ArrayEquals,
		Wart.ExplicitImplicitTypes,
		Wart.LeakingSealed,
		//Wart.Overloading,
		//Wart.PublicInference,
		Wart.TraversableOps,
	)
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
				"-language:higherKinds"//,
				// "-language:reflectiveCalls",
				// "-language:dynamics",
				// "-language:postfixOps",
				// "-language:experimental.macros",
				// "-Ymacro-debug-lite",
			),
			libraryDependencies	++= Seq(
				"org.scala-lang"	%	"scala-reflect"	% scalaVersion.value	% "provided",
				"org.specs2"		%%	"specs2-core"	% "4.3.5"				% "test"
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
		(project	in	file("modules/core"))
		.settings(
			fixConsoleSettings,
			scalacOptions	++= Seq(
				"-language:implicitConversions"
				// "-language:existentials",
				// "-language:higherKinds",
				// "-language:reflectiveCalls",
				// "-language:dynamics",
				// "-language:postfixOps",
				// "-language:experimental.macros",
				// "-Ymacro-debug-lite",
			),
			libraryDependencies	++= Seq(
				"org.scala-lang"	%	"scala-reflect"	% scalaVersion.value	% "provided",
				"org.specs2"		%%	"specs2-core"	% "4.3.5"				% "test"
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
		(project	in	file("modules/swing"))
		.settings(
			fixConsoleSettings,
			scalacOptions	++= Seq(
				"-language:implicitConversions"
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
		(project	in	file("modules/xml"))
		.settings(
			fixConsoleSettings,
			scalacOptions	++= Seq(
				// "-language:implicitConversions",
				// "-language:existentials",
				// "-language:higherKinds",
				// "-language:reflectiveCalls",
				// "-language:dynamics",
				// "-language:postfixOps",
				// "-language:experimental.macros",
			),
			libraryDependencies	++= Seq(
				"org.scala-lang.modules"	%% "scala-xml"	% "1.1.1"	% "compile"
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
				// "-language:higherKinds",
				// "-language:reflectiveCalls",
				// "-language:dynamics",
				// "-language:postfixOps",
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
				"org.scala-js"	%%%	"scalajs-dom"	% "0.9.6"	% "compile"
			)
		)
lazy val `scutil-guid-jvm`	= `scutil-guid`.jvm
lazy val `scutil-guid-js`	= `scutil-guid`.js
