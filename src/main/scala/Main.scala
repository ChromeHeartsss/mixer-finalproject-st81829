package fp

object Main {

  // sbt "run <seed> <addresses> <txCount> <mixerEvents> <groupSize>"
  def main(args: Array[String]): Unit = {
    val seed        = argLong(args, 0, 42L)
    val addresses   = argInt(args, 1, 30)
    val txCount     = argInt(args, 2, 120)
    val mixerEvents = argInt(args, 3, 8)
    val groupSize   = argInt(args, 4, 5)

    val genParams = GenParams(
      addresses = addresses,
      txCount = txCount,
      maxAmount = 5000L,
      startTs = 1_700_000_000L
    )

    val before = Generator.generate(seed, genParams)

    // mixerAddressId: put it outside normal range to be visually distinct
    val mixParams = MixerParams(
      events = mixerEvents,
      groupSize = groupSize,
      mixerAddressId = addresses + 999
    )

    val mixRes = MixerModel.applyMixer(seed, before, mixParams)
    val metrics = MetricsCalc.compute(mixRes.before, mixRes.after, mixRes.events)

    Exporter.writeText("graph_before.dot", Exporter.toDot(mixRes.before, "Before Mixer Model"))
    Exporter.writeText("graph_after.dot", Exporter.toDot(mixRes.after, "After Mixer Model"))
    Exporter.writeJson("metrics.json", metrics)
    Exporter.writeText(
        "mixer_subgraph.dot",
        Exporter.toMixerSubgraphDot(mixRes.after, mixParams.mixerAddressId, "Mixer Subgraph")
    )


    println("Done.")
    println(s"- graph_before.dot")
    println(s"- graph_after.dot")
    println(s"- metrics.json")
    println()
    println(s"Metrics: $metrics")
  }

  private def argInt(args: Array[String], idx: Int, default: Int): Int =
    if (args.length > idx) args(idx).toIntOption.getOrElse(default) else default

  private def argLong(args: Array[String], idx: Int, default: Long): Long =
    if (args.length > idx) args(idx).toLongOption.getOrElse(default) else default
}
