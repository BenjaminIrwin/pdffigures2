lazy val scala212 = "2.12.20"
lazy val scala213 = "2.13.15"
lazy val supportedScalaVersions = List(scala212, scala213)

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / organization := "org.allenai"
ThisBuild / description  := "Scala library to extract figures, tables, and captions from scholarly documents"
ThisBuild / scalaVersion := scala213
ThisBuild / version      := "0.1.0"
ThisBuild / semanticdbEnabled := true

scalacOptions ++= Seq(
  "-Wunused:imports",  // Required for OrganizeImports with removeUnused set to true
  "-Wunused"           // Required for RemoveUnused rule
)

lazy val projectSettings = Seq(
  name := "pdffigures2",
  crossScalaVersions := supportedScalaVersions,
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := { _ => false },
  licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
  homepage := Some(url("https://pdffigures2.allenai.org/")),
  scmInfo := Some(ScmInfo(
    url("https://github.com/allenai/pdffigures2"),
    "https://github.com/allenai/pdffigures2.git")),
  bintrayPackage := s"${organization.value}:${name.value}_${scalaBinaryVersion.value}",
  bintrayOrganization := Some("allenai"),
  bintrayRepository := "maven",
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    "io.spray" %% "spray-json" % "1.3.6",
    "com.github.scopt" %% "scopt" % "4.1.0",
    "ch.qos.logback" % "logback-classic" % "1.5.6",
    "org.slf4j" % "jcl-over-slf4j" % "2.0.13",
    "org.apache.pdfbox" % "pdfbox" % "3.0.2",
    "org.apache.pdfbox" % "fontbox" % "3.0.2",
    "com.typesafe" % "config" % "1.4.3",

    // So PDFBox can parse more image formats
    // These are disabled by default, because they are not licensed flexibly enough.
//    "com.github.jai-imageio" % "jai-imageio-core" % "1.4.0",
//    "com.github.jai-imageio" % "jai-imageio-jpeg2000" % "1.4.0", // For handling jpeg2000 images
//    "com.levigo.jbig2" % "levigo-jbig2-imageio" % "2.0", // For handling jbig2 images

    // So PDFBox can parse security enabled but still readable PDFs
    "org.bouncycastle" % "bcprov-jdk18on" % "1.78.1",
    "org.bouncycastle" % "bcmail-jdk18on" % "1.78.1",
    "org.bouncycastle" % "bcpkix-jdk18on" % "1.78.1",

    "org.scalatest" %% "scalatest" % "3.2.19" % Test
  ),

  pomExtra :=
      <developers>
        <developer>
          <id>allenai-dev-role</id>
          <name>Allen Institute for Artificial Intelligence</name>
          <email>dev-role@allenai.org</email>
        </developer>
      </developers>
)

lazy val root = (project in file("."))
    .settings(projectSettings)

Compile / run / mainClass := Some("org.allenai.pdffigures2.FigureExtractorBatchCli")
Compile / mainClass := Some("org.allenai.pdffigures2.FigureExtractorBatchCli")
assembly / mainClass := Some("org.allenai.pdffigures2.FigureExtractorBatchCli")
assembly / assemblyOutputPath := file("pdffigures2.jar")

assembly / assemblyMergeStrategy := {
  case x if x.endsWith("module-info.class") => MergeStrategy.discard
  case PathList("META-INF", "versions", "9", "OSGI-INF", "MANIFEST.MF") => MergeStrategy.first
  case PathList("org", "apache", "commons", xs @ _*) => MergeStrategy.first
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}
