name := "sangria-auth-example"
version := "0.1.0-SNAPSHOT"

description := "An example GraphQL server written with akka-http and sangria that authenticates users."

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.sangria-graphql" %% "sangria" % "0.5.1",
  "org.sangria-graphql" %% "sangria-spray-json" % "0.1.0",
  "com.typesafe.akka" %% "akka-http-experimental" % "2.0",
  "com.typesafe.akka" %% "akka-http-spray-json-experimental" % "2.0"
)