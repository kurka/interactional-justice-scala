package Network

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class Net(size: Int, nNeighbours: Int, nCheaters:Int, rndSeed: Int) {
  val rnd = new Random(rndSeed)
  val nei_offsets = (-nNeighbours/2 to (nNeighbours/2)).toSet
  val head = rnd.nextInt(size)
  val nNonCheaters = size - nCheaters

  val agents = ((0 until size) map { idx =>
    if (idx != head) Agent(idx, get_init_neigh(idx), rnd.nextLong())
    else Agent(idx, get_init_neigh(idx), rnd.nextLong(), head = true)
  }).toParArray

  def get_init_neigh(idx: Int) = (nei_offsets map {off => (idx+off+size)%size}) - idx


  def turn() = {
    //demand provide
    val step2Facts = agents map {_.new_turn()}
    val pool = step2Facts.map(_.available.get).sum
    //get allocation order
    val allocOrder = rnd.shuffle(agents zip step2Facts).toArray.sortWith(reputationDissatisfaction(_) > reputationDissatisfaction(_))
    val (remPool, allocations) = allocOrder.foldLeft((pool, List[Double]))({case ((remPool: Double, allocs: List[Double]), (ag: Agent, facts: InstitutionalFactsn)) => {
      val newAlloc = (math.min(facts.demanded.get, remPool)
      (remPool-newAlloc, allocs. :+ newAlloc)
    }})
  }


  def reputationDissatisfaction(agFacts: (Agent, InstitutionalFacts)) = {
    agFacts match { case (ag, _) =>

      val (ts, l) = ag.neighbours.foldLeft((0.0,0)) {
        case ((total, count), nei) => (total + agents(nei).trust(ag), count+1) }
      val reputation = ts/l
      val demand = 1 - ag.phi
      reputation*demand
    }
  }



  override def toString = agents.map(_.toString) mkString "\n"
}
