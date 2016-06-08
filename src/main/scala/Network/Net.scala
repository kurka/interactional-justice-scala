package Network

import Network.Net.Turn
import akka.actor.{Actor, Props}

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.mutable.ParArray
import scala.util.Random


class Net(size: Int, nNeighbours: Int, nCheaters:Int, rndSeed: Int) extends Actor {
  val rnd = new Random(rndSeed)
  val nei_offsets = (-nNeighbours/2 to (nNeighbours/2)).toSet
  val head = rnd.nextInt(size)
  val nNonCheaters = size - nCheaters

  val agents = (0 until size map { idx =>
    if (idx != head) Agent(idx, get_init_neigh(idx), rnd.nextLong())
    else Agent(idx, get_init_neigh(idx), rnd.nextLong(), head = true)
  }).toParArray

  def get_init_neigh(idx: Int) = (nei_offsets map {off => (idx+off+size)%size}) - idx

  def turn() = {
    //demand provide
    val step2Facts = agents map {_.new_turn()}
    val pool = step2Facts.map(_.available.get).sum
    //get allocation order
    val allocOrder = rnd.shuffle(agents.zip(step2Facts).toList).sortWith(reputationDissatisfaction(_) > reputationDissatisfaction(_))
    val (remPool, allocations) = allocOrder.foldLeft((pool, List[Double]()))({
      case ((remPool: Double, allocs: List[Double]), (ag: Agent, facts: InstitutionalFacts)) =>
        val newAlloc = math.min(facts.demanded.get, remPool)
        (remPool-newAlloc, allocs :+ newAlloc)
      })
    assert(remPool==0, "The pot wasn't completly distributed!")
    val finalFacts = rnd.shuffle(allocOrder zip allocations).toParArray.map {
      case ((ag, agfacts), alloc) => (ag, ag.receiveAllocations(alloc, agfacts))}
    val roundStats = finalFacts.map {case (ag, fFacts) => ag.updateOpinions(fFacts)}
    aggregateStats(roundStats)
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


  def aggregateStats(roundStats: ParArray[Stats]) = {
    val (allocations, satisfactions, utilities) = roundStats.map({
      case agStats => (agStats.totalAllocation/agStats.turnsPlayed, agStats.satisfaction, agStats.utility)}).toArray.unzip3
    AggregatedStats(allocations, satisfactions, utilities)
  }

  override def toString = agents.map(_.toString) mkString "\n"

  def receive = {
    case Turn(turnNumber) => {
      val turnStats = turn()
      sender() ! turnStats
    }
  }
}

object Net {
  case class Turn(turnNumber: Int)
  def props(size: Int, nNeighbours: Int, nCheaters:Int, rndSeed: Int): Props = Props(new Net(size, nNeighbours, nCheaters, rndSeed))
}

//class Greeter extends Actor {
//  var greeting = ""
//
//  def receive = {
//    case WhoToGreet(who) => greeting = s"hello, $who"
//    case Greet           => sender ! Greeting(greeting) // Send the current greeting back to the sender
//  }
//}