if (sys.props("java.version").startsWith("1.6"))
  addSbtPlugin("org.ensime" % "sbt-ensime" % "1.0.0")
else
  addSbtPlugin("org.ensime" % "sbt-ensime" % "1.11.0")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.2.0")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")
