organization	:= "de.djini"

name			:= "scutil"

version			:= "0.44.0"

organization	in ThisBuild	:= organization.value

version			in ThisBuild	:= version.value

scalaVersion	in ThisBuild	:= "2.11.0"

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
