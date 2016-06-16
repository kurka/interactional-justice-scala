package Network

import Network.Agent._

import scala.util.Random
import Network.CheatMode._
import Network.Net._
import akka.actor.{Actor, ActorRef, Props}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class Agent(id: Int, rndSeed: Long, headAddr: ActorRef, cheater:Boolean = false, var isHead:Boolean = false) extends Actor {
  var neighbours = Set.empty[ActorRef]
  val rnd = new Random(rndSeed)
  var facts = InstitutionalFacts(rnd, None, None, None, None, None, None)
  var claims = Claims(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  var stats = new Stats(turnsPlayed = 0, totalAllocation = 0.0, satisfaction = 0.5, utility = 0.0)

  val neighboursClaims = ArrayBuffer.empty[(ActorRef, Claims)]//mutable.Map.empty[ActorRef, Claims]
  val neighboursTrusts = neighbours.map(_ -> mutable.Map.empty[ActorRef, Double]).toMap
  var trustsReceived = 0
  var trusts: Map[ActorRef, Double] = neighbours.map(_ -> 1.0).toMap

  val gamma = 0.1
  val MAX_PROP_ITERS = 50

  def newTurn() = {
    //TODO: update agent status and roles
    //set need and demand
    val initialFacts = facts.erase()

    val step1Facts = initialFacts.updateAvailableDemanded()
    //define demand and provision
    val step2Facts = step1Facts.updateDemandProvision(shouldCheat = false, NoCheat) //TODO: implement cheating
    facts = step2Facts
    //tell neighbors your opinions //TODO: should this be done at a different stage?
    neighbours.foreach({nei => nei ! PropagatedOpinion(this.claims)})
//    neighbours.foreach({nei => nei ! 2})

//    val fakeTrusts = neighbours.map(_ -> 0.0).toMap //FIXME
//    OpinionAndTrust(self, phi, fakeTrusts, facts)
  }

  def getAccordances(neighbourClaims: Map[ActorRef, Claims]): Map[ActorRef, Double] = {
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
//      neighboursClaims.clear() //TODO: what happen if Opinion message arrives before StartTurn?
      newTurn()


    case PropagatedOpinion(neiClaim) =>
      neighboursClaims += (sender -> neiClaim)
      println(self.toString() + neighboursClaims.size)
      if(neighboursClaims.size == neighbours.size) {
        // calculate accordances
        val accordances = getAccordances(neighboursClaims.toMap)
        trusts = updateTrusts(accordances)
        //TODO: trust propagation!!!
        trustsReceived = 0
        neighboursTrusts.values.foreach(_.empty)
        //        neighbours.foreach(_ ! PropagatedTrust(trusts, 0))
        //FIXME: test and then remove
        headAddr ! OpinionAndTrust(this.claims.getPhi, this.trusts, this.facts)
      }
      else if(neighboursClaims.size > neighbours.size)
        println("what???")

//    case PropagatedTrust(neiTrusts, it) =>
//      trustsReceived += 1
//      for ((neiNei, neiTrust) <- neiTrusts
//           if neighbours.contains(neiNei)) {
//        neighboursTrusts(neiNei)(sender) = neiTrust}
//
//      if (trustsReceived == neighbours.size) {
//        // add my own trust to neighbourTrusts mapping
//        this.trusts.foreach({case (nei, myTrust) => neighboursTrusts(nei)(self) = myTrust})
//        // assume that self trust is equal to 1 (maximum)
//        val trustsWithSelfTrust = trusts + (self -> 1.0)
//        val trustsSum = trustsWithSelfTrust.values.sum
//        val normTrustsWithSelfTrust = trustsWithSelfTrust.map({case (a,t) => a -> t/trustsSum})
//        // t_ij' = sum(t_ik*tkj)
//        val updatedTrusts = neighboursTrusts.map({
//          case (j, jTrusts) => j -> jTrusts.map({case (k, tkj) => normTrustsWithSelfTrust(k)*tkj}).sum})
//
//        trusts = updatedTrusts
//
////        if (it < MAX_PROP_ITERS) //TODO: check other stop conditions - diff
//////        neighbours.foreach(_ ! PropagatedTrust(trusts, it+1)) else //TODO: check how to verify iterations
//        headAddr ! OpinionAndTrust(this.claims.getPhi, this.trusts, this.facts)
//      }


    case Allocation(alloc) =>
      receiveAllocations(alloc)
      val turnOpinion = updateOpinions()
      sender ! TurnFinished(turnOpinion)
      neighboursClaims.clear()

    case x: Any => println("Error! Message not identified!\n" + x.toString)


  }
}


object Agent {
  case class Allocation(alloc: Double)
  case class OpinionAndTrust(opinion: Double, trusts: Map[ActorRef, Double], facts: InstitutionalFacts)
  case class UpdateNeighbours(newNeighbours: Set[ActorRef])
  case class PropagatedOpinion(claims: Claims)
  case class PropagatedTrust(trusts: Map[ActorRef, Double], iteration: Int)
  def props(id: Int, rndSeed: Long, headAddr:ActorRef, cheater:Boolean = false, head:Boolean = false): Props =
    Props(new Agent(id, rndSeed, headAddr, cheater, head))
}