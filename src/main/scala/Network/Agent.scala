package Network

import Network.Agent.{Allocation, OpinionAndTrust}

import scala.util.Random
import Network.CheatMode._
import Network.Net.{StartTurn, TurnFinished}
import akka.actor.{Actor, ActorRef, Props}


class Agent(id: Int, var neighbours: Set[Int], rndSeed: Long,
                 cheater:Boolean = false, var head:Boolean = false) extends Actor {
  val rnd = new Random(rndSeed)
  var facts = InstitutionalFacts(rnd, None, None, None, None, None, None)
  var claims = Claims(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  var stats = new Stats(turnsPlayed = 0, totalAllocation = 0.0, satisfaction = 0.5, utility = 0.0)
  def phi = claims.getPhi

  def newTurn(): OpinionAndTrust = {
    //TODO: update agent status and roles
    //set need and demand
    val initialFacts = facts.erase()

    val step1Facts = initialFacts.updateAvailableDemanded()
    //define demand and provision
    val step2Facts = step1Facts.updateDemandProvision(shouldCheat = false, NoCheat) //TODO: implement cheating
    facts = step2Facts
    val fakeTrusts = neighbours.map(_ -> 0.0).toMap //FIXME
    OpinionAndTrust(self, phi, fakeTrusts, facts)
  }

  def receiveAllocations(turnAlloc: Double) = {
    facts = facts.updateAllocatedAppropriated(turnAlloc, shouldCheat = false, NoCheat) //TODO: implement cheating
  }

  def trust(neig: Agent): Double = {
    assert(neighbours contains neig.id, "trying to compute trust of non-connected agent!")
    (neig.claims.allClaims, this.claims.allClaims).zipped.map({
      (nc, tc) => logistic(math.abs(nc - tc), k=15, thresh=0.25)
    }).sum
    //TODO: reinforcement learning, looking to past!!!
    //TODO: compare to environment (eq 4b)
    //TODO: trust propagation!!!
  }

  def logistic(x: Double, k:Double = 15, thresh: Double = 0.25) =
    1 - (1 / (1 + math.exp(-k*(x-thresh))))

  def updateOpinions() = {
    val (newStats, newClaims) = claims.legitimateClaims(facts, stats, head)
    stats = newStats
    claims = newClaims
    stats
  }

  def receive = {
    case StartTurn(turnNumber) => {
      val opinionAndTrusts = newTurn()
      sender() ! opinionAndTrusts
    }
    case Allocation(alloc) => {
      receiveAllocations(alloc)
      val turnOpinion = updateOpinions()
      sender() ! TurnFinished(turnOpinion)
    }

  }
}


object Agent {
  case class Allocation(alloc: Double)
  case class OpinionAndTrust(ag: ActorRef, opinion: Double, trusts: Map[Int, Double], facts: InstitutionalFacts)
  def props(id: Int, neighbours: Set[Int], rndSeed: Long, cheater:Boolean = false, head:Boolean = false): Props =
    Props(new Agent(id, neighbours, rndSeed, cheater, head))
}



//val allocOrder = rnd.shuffle(agents.zip(step2Facts).toList).sortWith(reputationDissatisfaction(_) > reputationDissatisfaction(_))
//val (remPool, allocations) = allocOrder.foldLeft((pool, List[Double]()))({
//case ((remPool: Double, allocs: List[Double]), (ag: Agent, facts: InstitutionalFacts)) =>
//val newAlloc = math.min(facts.demanded.get, remPool)
//(remPool-newAlloc, allocs :+ newAlloc)
//})
//assert(remPool==0, "The pot wasn't completly distributed!")
//val finalFacts = rnd.shuffle(allocOrder zip allocations).toParArray.map {
//case ((ag, agfacts), alloc) => (ag, ag.receiveAllocations(alloc, agfacts))}
//val roundStats = finalFacts.map {case (ag, fFacts) => ag.updateOpinions(fFacts)}
//aggregateStats(roundStats)