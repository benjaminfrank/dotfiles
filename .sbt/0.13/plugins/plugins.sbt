addSbtPlugin("org.ensime" % "ensime-sbt" % "0.1.6")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.8")

// recommended, but no gpgAgent option?
// https://github.com/sbt/sbt-pgp/issues/77
//addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.3")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.5")
