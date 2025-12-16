package fp

import scala.util.Random

final case class GenParams(
  addresses: Int,
  txCount: Int,
  maxAmount: Long,
  startTs: Long
) derives CanEqual

object Generator {

  def generate(seed: Long, p: GenParams): Graph = {
    val rng = new Random(seed)

    val addrs = Vector.tabulate(p.addresses)(Address.apply)

    val txs = Vector.tabulate(p.txCount) { i =>
      val from = addrs(rng.nextInt(addrs.size))
      // ensure to != from when possible
      val to = if (addrs.size <= 1) from else {
        val candidate = addrs(rng.nextInt(addrs.size))
        if (candidate == from) addrs((candidate.id + 1) % addrs.size) else candidate
      }
      val amount = 1L + (rng.nextLong().abs % p.maxAmount)
      val ts = p.startTs + i.toLong
      Tx(from, to, amount, ts)
    }

    Graph(txs)
  }
}
