package Network

import scala.collection.mutable.ArrayBuffer

class Net(size: Int, nCheaters:Int, r:scala.util.Random) {
  val n_nei = 20
  val nei_offsets = (-n_nei/2 to (n_nei/2)).toSet
  val agents = ArrayBuffer.range(0, size) map {idx => Agent(idx, get_init_neigh(idx))}
  def get_init_neigh(idx: Int) = (nei_offsets map {off => (idx+off+size)%size}) - idx

  val nNonCheaters = size - nCheaters

  def assignRandomHead() = {
    val randIdx = r.nextInt(size)
    agents.update(randIdx, agents(randIdx).setHead())
  }

  def demandProvide() = {
    val demands = agents map _.demand

  }

  override def toString = agents.map(_.toString) mkString "\n"
}
