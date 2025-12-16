package fp

import upickle.default.*

object Exporter {

  given ReadWriter[Metrics] = macroRW

  def toDot(g: Graph, title: String): String = {
    val header =
      s"""digraph G {
         |  label="${escape(title)}";
         |  labelloc=top;
         |  fontsize=20;
         |""".stripMargin

    val edges = g.txs.map { t =>
      val from = s"a${t.from.id}"
      val to   = s"a${t.to.id}"
      s"""  $from -> $to [label="$$${t.amount}"];"""
    }.mkString("\n")

    val footer = "\n}\n"
    header + edges + footer
  }

  def toMixerSubgraphDot(after: Graph, mixerId: Int, title: String): String = {
    val mixer = Address(mixerId)
    val onlyMixerEdges = after.txs.filter(t => t.from == mixer || t.to == mixer)
    toDot(Graph(onlyMixerEdges), title)
  }

  def writeText(path: String, content: String): Unit = {
    val p = java.nio.file.Paths.get(path)
    java.nio.file.Files.writeString(p, content, java.nio.charset.StandardCharsets.UTF_8)
  }

  def writeJson(path: String, m: Metrics): Unit = {
    val json = write(m, indent = 2)
    writeText(path, json + "\n")
  }

  private def escape(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"")
}
