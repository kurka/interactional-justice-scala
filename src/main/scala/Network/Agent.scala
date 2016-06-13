package Network

import Network.Agent._

import scala.util.Random
import Network.CheatMode._
import Network.Net._
import akka.actor.{Actor, ActorRef, Props}

import scala.collection.mutable


class Agent(id: Int, rndSeed: Long, headAddr: ActorRef, cheater:Boolean = false, var isHead:Boolean = false) extends Actor {
  var neighbours = Set.empty[ActorRef]
  val rnd = new Random(rndSeed)
  var facts = InstitutionalFacts(rnd, None, None, None, None, None, None)
  var claims = Claims(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  var stats = new Stats(turnsPlayed = 0, totalAllocation = 0.0, satisfaction = 0.5, utility = 0.0)
  def phi = claims.getPhi
  val neighbourClaims = mutable.Map.empty[ActorRef, Claims]
//  val neighbourAccordances = scala.collection.mutable.Map.empty[ActorRef, Claims]
  var trusts: Map[ActorRef, Double] = neighbours.map(_ -> 1.0).toMap

  val gamma = 0.1

  def newTurn() = {
    //TODO: update agent status and roles
    //set need and demand
    val initialFacts = facts.erase()

    val step1Facts = initialFacts.updateAvailableDemanded()
    //define demand and provision
    val step2Facts = step1Facts.updateDemandProvision(shouldCheat = false, NoCheat) //TODO: implement cheating
    facts = step2Facts
    //tell neighbors your opinions //TODO: should this be done at a different stage?
    neighbours.foreach(_ ! Opinion(this.claims))

    val fakeTrusts = neighbours.map(_ -> 0.0).toMap //FIXME
//    OpinionAndTrust(self, phi, fakeTrusts, facts)
  }

  def getAccordances(neighbourClaims: mutable.Map[ActorRef, Claims]): Map[ActorRef, Double] = {
    val contextClaimsSums = neighbourClaims.values.foldLeft (this.claims) (_+_)
    val contextSize = neighbourClaims.size

    neighbours.map({nei => {
      val myContextOpinion = (contextClaimsSums - neighbourClaims(nei))/(contextSize-1)
      nei -> neighbourClaims(nei).accordance(myContextOpinion)
    }}).toMap
  }

  def updateTrusts(turnAccordances: Map[ActorRef, Double]): Map[ActorRef, Double] = {
    trusts.map({ case (ag, oldTrust) => ag -> ((1 - gamma) * oldTrust + gamma * turnAccordances(ag)) })
  }

  def receiveAllocations(turnAlloc: Double) = {
    facts = facts.updateAllocatedAppropriated(turnAlloc, shouldCheat = false, NoCheat) //TODO: implement cheating
  }

//  def computeTrusts(): Map[ActorRef, Double] = {
//    // trust assessments
//    //TODO: self-trust
////    val neighboursOpinions = neighbours.map {(_ ? AskOpinion).mapTo[Double] }
////    neighboursOpinions
//  }

  def updateOpinions() = {
    val (newStats, newClaims) = claims.legitimateClaims(facts, stats, isHead)
    stats = newStats
    claims = newClaims
    stats
  }

  def receive = {
    case UpdateNeighbours(newNeighbours) =>
      neighbours = newNeighbours

    case StartTurn(turnNumber) =>
      neighbourClaims.empty //TODO: what happen if Opinion message arrives before StartTurn?
      val opinionAndTrusts = newTurn()


    case Opinion(neiClaim) =>
      neighbourClaims(sender) += neiClaim
      if(neighbourClaims.keys.size == neighbours.size) {
        // calculate accordances
        val accordances = getAccordances(neighbourClaims)
        trusts = updateTrusts(accordances)
        //TODO: trust propagation!!!
        headAddr ! OpinionAndTrust(this.phi, this.trusts, this.facts)
      }


    case Allocation(alloc) =>
      receiveAllocations(alloc)
      val turnOpinion = updateOpinions()
      sender ! TurnFinished(turnOpinion)



  }
}


object Agent {
  case class Allocation(alloc: Double)
  case class OpinionAndTrust(opinion: Double, trusts: Map[ActorRef, Double], facts: InstitutionalFacts)
  case class UpdateNeighbours(newNeighbours: Set[ActorRef])
  case class Opinion(claims: Claims)
  case class GetTrust(ag: ActorRef)
  def props(id: Int, rndSeed: Long, headAddr:ActorRef, cheater:Boolean = false, head:Boolean = false): Props =
    Props(new Agent(id, rndSeed, headAddr, cheater, head))
}