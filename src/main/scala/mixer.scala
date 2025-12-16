package fp

import scala.util.Random

final case class MixerParams(
  events: Int,          // how many mixer events
  groupSize: Int,       // k inputs -> k outputs
  mixerAddressId: Int   // a dedicated "mixer" address id in the synthetic graph
) derives CanEqual

final case class MixerEvent(
  inputs: Vector[Tx],   // txs going into mixer
  outputs: Vector[Tx],  // txs leaving mixer
  k: Int
) derives CanEqual

final case class MixerResult(
  before: Graph,
  after: Graph,
  events: Vector[MixerEvent]
) derives CanEqual

object MixerModel {

  /**
    * Mixer-like transform on synthetic graph:
    * - Select k existing transactions as "inputs"
    * - Route them into a dedicated mixer address
    * - Emit k outputs from the mixer to k recipient addresses (shuffled)
    *
    * We keep amounts and timestamps simple and deterministic-ish.
    */
  def applyMixer(seed: Long, g: Graph, p: MixerParams): MixerResult = {
    val rng = new Random(seed ^ 0x9E3779B97F4A7C15L)

    val mixer = Address(p.mixerAddressId)
    val txs = g.txs

    val safeGroup = math.max(2, p.groupSize)
    val maxEvents = math.max(0, p.events)

    // choose indices of candidate txs to form input groups
    val candidateIdxs: Vector[Int] =
      if (txs.isEmpty) Vector.empty
      else Vector.fill(maxEvents * safeGroup)(rng.nextInt(txs.size))

    val groups: Vector[Vector[Tx]] =
      candidateIdxs.grouped(safeGroup).map(_.toVector.map(txs)).toVector

    val events: Vector[MixerEvent] = groups.zipWithIndex.map { case (ins, ei) =>
      val k = ins.size

      // Inputs: from original senders -> mixer
      val inputsToMixer: Vector[Tx] =
        ins.zipWithIndex.map { case (t, j) =>
          // preserve sender, route to mixer
          Tx(from = t.from, to = mixer, amount = t.amount, ts = t.ts)
        }

      // Outputs: mixer -> recipients (shuffle recipients)
      val recipients: Vector[Address] = rng.shuffle(ins.map(_.to)).toVector

      val outputsFromMixer: Vector[Tx] =
        recipients.zipWithIndex.map { case (toAddr, j) =>
          // spread timestamps slightly after last input ts in this group
          val baseTs = ins.map(_.ts).maxOption.getOrElse(0L)
          Tx(from = mixer, to = toAddr, amount = ins(j).amount, ts = baseTs + 1 + j.toLong)
        }

      MixerEvent(inputs = inputsToMixer, outputs = outputsFromMixer, k = k)
    }

    // Build "after" graph: keep original txs + add mixer in/out txs
    // (For a more aggressive transform, we could remove/rewrite original edges;
    //  but базовая версия — добавляет миксер-события и демонстрирует эффект на ASS/TLS.)
    val injected: Vector[Tx] = events.flatMap(e => e.inputs ++ e.outputs)
    val after = g.addAll(injected)

    MixerResult(before = g, after = after, events = events)
  }
}
