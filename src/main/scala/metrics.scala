package fp

final case class Metrics(
  txCountBefore: Int,
  txCountAfter: Int,
  mixerEvents: Int,
  avgAnonymitySetSize: Double,
  traceabilityLossScore: Double
) derives CanEqual

object MetricsCalc {

  /**
    * Average Anonymity Set Size (ASS) computed from mixer events: mean(k).
    */
  def avgASS(events: Vector[MixerEvent]): Double =
    if (events.isEmpty) 1.0
    else events.map(_.k.toDouble).sum / events.size.toDouble

  /**
    * Simple traceability loss score: 1 - (1 / avgASS)
    * - 0.0 means no loss (ASS=1)
    * - approaches 1.0 as ASS grows
    */
  def tls(avgAss: Double): Double =
    if (avgAss <= 1.0) 0.0 else 1.0 - (1.0 / avgAss)

  def compute(before: Graph, after: Graph, events: Vector[MixerEvent]): Metrics = {
    val a = avgASS(events)
    Metrics(
      txCountBefore = before.txs.size,
      txCountAfter = after.txs.size,
      mixerEvents = events.size,
      avgAnonymitySetSize = a,
      traceabilityLossScore = tls(a)
    )
  }
}
