name := "xml"

scalaVersion := "2.11.4"

scalacOptions += "-deprecation"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.0.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

scalacOptions += "-Xplugin:/home/swachter/.ivy/local/demo/boxer_2.11/0.1-SNAPSHOT/jars/boxer_2.11.jar"

// scalacOptions += "-Xshow-phases"
