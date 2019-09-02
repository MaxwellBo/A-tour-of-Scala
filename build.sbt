scalaVersion := "2.13.0"
name := "a-tour-of-scala"
organization := "com.atlassian"
version := "1.0"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
