val sharedSettings = Seq(
  scalaVersion := "3.8.1",
  scalacOptions ++= Seq(
    "-new-syntax",
    // "-no-indent",
    "-Wvalue-discard",
    "-Wunused:all",
    // "-Werror",
    "-deprecation",
    "-explain",
    "-explain-cyclic",
    "-rewrite",
    "-preview",
    "-source:future",
    "-Wunused:patvars" // unused pattern variables
  ),
  javacOptions ++= Seq("-source", "25", "-target", "25")
)

lazy val vigilo =
  // select supported platforms
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Full) // [Pure, Full, Dummy], default: CrossType.Full
    .withoutSuffixFor(JVMPlatform)
    .in(file("vigilo"))
    .settings(sharedSettings *)
    .settings(name := "vigilo", organization := "io.vigilo", version := "0.0.1")
    .jvmSettings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test")
    // configure Scala-Native settings
    .nativeSettings( /* ... */ ) // defined in sbt-scala-native
