addSbtPlugin("org.ensime" % "sbt-ensime" % "1.12.9")

// 0.3.0 uses Java 7 https://github.com/rtimush/sbt-updates/issues/71
if (sys.props("java.version").startsWith("1.6"))
  addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.2.0")
else
  addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.0")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")
