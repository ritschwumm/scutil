import spray.boilerplate.BoilerplatePlugin
import sbtcrossproject.{ CrossProject, CrossType, Platform }

Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(Seq(
	organization	:= "de.djini",
	version			:= "0.250.0",

	scalaVersion	:= "3.5.0",
	scalacOptions	++= Seq(
		"-feature",
		"-deprecation",
		"-unchecked",
		"-source:future",
		// "-explain",
		"-Wunused:all",
		"-Xfatal-warnings",
		"-Xkind-projector:underscores",
	),

	versionScheme	:= Some("early-semver"),

	resolvers	++= Resolver sonatypeOssRepos "releases",

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
		// TODO get this back - for some reason, it cannot be disabled
		//Wart.ImplicitParameter,
		Wart.ExplicitImplicitTypes,
		Wart.LeakingSealed,
		Wart.DefaultArguments,
		//Wart.Overloading,
		//Wart.PublicInference,
		//Wart.TraversableOps,
		Wart.ListUnapply,
		Wart.ListAppend,
		Wart.GlobalExecutionContext,
		Wart.PlatformDefault,
	)
))

// TODO add this here? Compile / console / scalacOptions := (console / scalacOptions).value.filterNot(_.contains("wartremover"))
lazy val fixConsoleSettings	=
	Seq(
		Compile / console / scalacOptions ~= (opts => opts filterNot Set(
			"-Wunused:all",
			"-Xfatal-warnings",
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
	project.in(file("."))
	.aggregate(
		`scutil-core-jvm`,
		`scutil-core-js`,
		`scutil-jdk`,
		`scutil-gui`,
		`scutil-xml`,
		`scutil-guid-jvm`,
		`scutil-guid-js`
	)
	.settings(
		publishArtifact	:= false
	)

//------------------------------------------------------------------------------

lazy val `scutil-core`	=
	myCrossProject("scutil-core", file("modules/core"), CrossType.Pure)
	.enablePlugins(
		BoilerplatePlugin
	)
	.settings(
		fixConsoleSettings,
		scalacOptions	++= Seq(),
		libraryDependencies	++= Seq(
			"io.monix"			%%	"minitest"		% "2.9.6"				% "test"
		),
		testFrameworks	+= new TestFramework("minitest.runner.Framework"),
		Compile / boilerplateSource	:= baseDirectory.value.getParentFile / "src" / "main" / "boilerplate"
	)
	.jvmSettings()
	.jsSettings(
		noTestSettings
	)
lazy val `scutil-core-jvm`	= `scutil-core`.jvm
lazy val `scutil-core-js`	= `scutil-core`.js

lazy val `scutil-jdk`	=
	project.in(file("modules/jdk"))
	.settings(
		fixConsoleSettings,
		scalacOptions	++= Seq(),
		libraryDependencies	++= Seq(
			"io.monix"			%%	"minitest"		% "2.9.6"				% "test"
		),
		testFrameworks	+= new TestFramework("minitest.runner.Framework"),

		//------------------------------------------------------------------------------

		console / initialCommands	:= """
			import java.io.File
			import java.nio.file.Path
			import java.nio.file.Files
			import scutil.lang._
			import scutil.lang.Charsets.{ us_ascii, iso_8859_1, utf_8 }
			import scutil.core.implicits._
			import scutil.jdk.implicits._
			import scutil.codec.Base64
			import scutil.codec.URIComponent
			import scutil.text.Human
			import scutil.time._
			import scutil.math._
			import scutil.number._
			import scutil.platform._
			import scutil.platform.Platform._
			import scutil.io.MoreFiles
		"""
	)
	.dependsOn	(`scutil-core-jvm`)

lazy val `scutil-gui`	=
	project.in(file("modules/gui"))
	.settings(
		fixConsoleSettings,
		scalacOptions	++= Seq(),
		libraryDependencies	++= Seq(
			"io.monix"			%%	"minitest"		% "2.9.6"				% "test"
		),
		testFrameworks	+= new TestFramework("minitest.runner.Framework"),
	)
	.dependsOn(
		`scutil-jdk`
	)

lazy val `scutil-xml`	=
	project.in(file("modules/xml"))
	.settings(
		fixConsoleSettings,
		scalacOptions	++= Seq(),
		libraryDependencies	++= Seq(
			"org.scala-lang.modules"	%% "scala-xml"	% "2.3.0"	% "compile"
		)
	)
	.dependsOn(
		`scutil-jdk`
	)

lazy val `scutil-guid`	=
	myCrossProject("scutil-guid", file("modules/guid"), CrossType.Full)
	.settings(
		fixConsoleSettings,
		scalacOptions	++= Seq(),
	)
	.dependsOn(
		`scutil-core`
	)
	.jvmSettings()
	.jsSettings(
		noTestSettings,
		libraryDependencies	++= Seq(
			"org.scala-js"	%%%	"scalajs-dom"	% "2.8.0"	% "compile"
		)
	)
lazy val `scutil-guid-jvm`	= `scutil-guid`.jvm
lazy val `scutil-guid-js`	= `scutil-guid`.js
