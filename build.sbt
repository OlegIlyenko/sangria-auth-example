name := "sangria-auth-example"
version := "0.1.0-SNAPSHOT"

description := "An example GraphQL server written with akka-http and sangria that authenticates users."

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.sangria-graphql" %% "sangria" % "0.3.0",
  "com.typesafe.akka" %% "akka-http-experimental" % "1.0",
  "org.json4s" %% "json4s-native" % "3.2.11",
  "de.heikoseeberger" %% "akka-http-json4s" % "1.0.0"
)