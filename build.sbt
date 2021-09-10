name := "poker-hand-evaluator"

version := "0.1"

scalaVersion := "2.13.3"

mainClass in (assembly) := Some("com.evolutiongaming.bootcamp.assignment.poker.Main")
//artifactPath in packageBin in Compile := baseDirectory.value / "jars"
assemblyJarName in assembly := "poker-hand-evaluator.jar"
crossTarget := baseDirectory.value / "lib"