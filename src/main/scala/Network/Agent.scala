package Network

import scala.util.Random
import Network.CheatMode._


case class Agent(id: Int, var neighbours: Set[Int], rndSeed: Long,
                 //attributes
                 var reputation:Double = 0.0,
                 var cheater:Boolean = false,
                 var head:Boolean = false,
                 var utility:Double = 0.0) {
  val rnd = new Random(rndSeed)
//  var facts = InstitutionalFacts(rnd)
  var claims = Claims()
  var stats = Stats()
  def phi = claims.getPhi

  def new_turn(): Unit = {
    //TODO: update agent status and roles
    //set need and demand
    val initialFacts = InstitutionalFacts(rnd, None, None, None, None, None, None)

    val step1Facts = initialFacts.updateAvailableDemanded()
    //define demand and provision
    val step2Facts = step1Facts.updateDemandProvision(shouldCheat = false, NoCheat) //TODO: implement cheating
    step2Facts
  }

  def receiveAllocations(turnAlloc: Double, step2Facts: InstitutionalFacts) = {
    val finalFacts = step2Facts.updateAllocatedAppropriated(turnAlloc, shouldCheat = false, NoCheat) //TODO: implement cheating
    finalFacts
  }

//  def updateOpinions() = {
//    claims = claims.updateClaims(facts)
//    //biuld trust
//    //propagate trust
//  }
}


