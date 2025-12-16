package fp

final case class Address(id: Int) derives CanEqual
final case class Tx(from: Address, to: Address, amount: Long, ts: Long) derives CanEqual

final case class Graph(txs: Vector[Tx]) derives CanEqual {
  def addAll(more: Vector[Tx]): Graph = Graph(txs ++ more)
}
