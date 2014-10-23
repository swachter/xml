name := "xml"

scalaVersion := "2.11.2"

scalacOptions += "-deprecation"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

scalacOptions += "-Xplugin:/home/swachter/.ivy/local/demo/boxer_2.11/0.1-SNAPSHOT/jars/boxer_2.11.jar"

// scalacOptions += "-Xshow-phases"
