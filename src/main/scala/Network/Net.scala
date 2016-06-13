package Network

import Network.Agent.{Allocation, OpinionAndTrust, UpdateNeighbours}
import Network.Net.{NewGame, StartTurn, TurnFinished}
import akka.actor.{Actor, ActorRef, Props}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class Net(size: Int, nNeighbours: Int, nCheaters:Int, rndSeed: Int, inbox: ActorRef) extends Actor {
  val rnd = new Random(rndSeed)
  val head = rnd.nextInt(size)
  val nNonCheaters = size - nCheaters

  val agentsRepDis = ArrayBuffer.empty[(ActorRef, Double)]
  val agentsFacts = scala.collection.mutable.Map.empty[ActorRef, InstitutionalFacts]
  var pool = 0.0
  val statsAggregator = ArrayBuffer.empty[Stats]

  val agents = 0 until size map {idx => context.actorOf(Agent.props(idx, rnd.nextLong(), self, head = idx == head))} //TODO: change the head to be really an agent

  lazy val nei_offsets = (-nNeighbours/2 to (nNeighbours/2)).toSet
  def getInitNeigh(idx: Int) = ((nei_offsets map { off => (idx+off+size)%size}) - idx).map {agents(_)}

  //  def turn() = {
  //    //demand provide
  //    val step2Facts = agents map {_.newTurn()}
  //    val pool = step2Facts.map(_.available.get).sum
  //    //get allocation order
  //    val allocOrder = rnd.shuffle(agents.zip(step2Facts).toList).sortWith(reputationDissatisfaction(_) > reputationDissatisfaction(_))
  //    val (remPool, allocations) = allocOrder.foldLeft((pool, List[Double]()))({
  //      case ((remPool: Double, allocs: List[Double]), (ag: Agent, facts: InstitutionalFacts)) =>
  //        val newAlloc = math.min(facts.demanded.get, remPool)
  //        (remPool-newAlloc, allocs :+ newAlloc)
  //      })
  //    assert(remPool==0, "The pot wasn't completly distributed!")
  //    val finalFacts = rnd.shuffle(allocOrder zip allocations).toParArray.map {
  //      case ((ag, agfacts), alloc) => (ag, ag.receiveAllocations(alloc, agfacts))}
  //    val roundStats = finalFacts.map {case (ag, fFacts) => ag.updateOpinions(fFacts)}
  //    aggregateStats(roundStats)
  //  }


  //  def reputationDissatisfaction(agFacts: (Agent, InstitutionalFacts)) = {
  //    agFacts match {
  //      case (ag, _) =>
  //        val (ts, l) = ag.neighbours.foldLeft((0.0, 0)) {
  //          case ((total, count), nei) => (total + agents(nei).trust(ag), count + 1)
  //        }
  //        val reputation = ts / l
  //        val demand = 1 - ag.phi
  //        reputation * demand
  //    }
  //  }


  def aggregateStats(roundStats: ArrayBuffer[Stats]) = {
    val (allocations, satisfactions, utilities) = roundStats.map({
      case agStats => (agStats.totalAllocation/agStats.turnsPlayed, agStats.satisfaction, agStats.utility)}).toArray.unzip3
    AggregatedStats(allocations, satisfactions, utilities)
  }

  override def toString = agents.map(_.toString) mkString "\n"

  def receive = {
    case NewGame =>
      for ((ag, idx) <- agents.zipWithIndex) {ag ! UpdateNeighbours(getInitNeigh(idx))}
    case StartTurn(turnNumber) =>
      agents.foreach(_ ! StartTurn(turnNumber))
      agentsRepDis.clear()
      pool = 0.0
      agentsFacts.clear()
      statsAggregator.clear()


    case OpinionAndTrust(opinion, trusts, facts) =>
      agentsRepDis += (sender -> (1-opinion)) //TODO: handle trust
      agentsFacts(sender) = facts
      pool += facts.provided.get
      if(agentsRepDis.length == agents.length){
        val allocOrder = rnd.shuffle(agentsRepDis).sortWith(_._2 > _._2)
        // perform allocation
        val (remPool, allocations) = allocOrder.foldLeft((pool, List[Double]()))({
          case ((remPool: Double, allocs: List[Double]), (ag: ActorRef, _)) =>
            val newAlloc = math.min(agentsFacts(ag).demanded.get, remPool)
            (remPool-newAlloc, allocs :+ newAlloc)
        })
        assert(remPool==0, "The pot wasn't completly distributed!")
        rnd.shuffle(allocOrder zip allocations).foreach({
          case ((ag: ActorRef, _), alloc: Double) => ag ! Allocation(alloc) // FIXME: this doesnt work with cheating in appropriation!!
        })
      }

    case TurnFinished(roundStats) =>
      statsAggregator += roundStats
      if(statsAggregator.length == agents.length) inbox ! aggregateStats(statsAggregator)
  }
}

object Net {
  case object NewGame
  case class StartTurn(turnNumber: Int)
  case class TurnFinished(roundStats: Stats)
  def props(size: Int, nNeighbours: Int, nCheaters:Int, rndSeed: Int, inbox: ActorRef): Props = Props(new Net(size, nNeighbours, nCheaters, rndSeed, inbox))
}