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
  var facts = InstitutionalFacts(rnd)
  var claims = Claims()
  var stats = Stats()
  def phi = claims.getPhi

  def new_turn(): Unit = {
    //TODO: update agent status and roles
    //set need and demand
    facts = facts.update_available_demanded()
    //define demand and provision
    facts = facts.update_demand_provision(false, None) //TODO: implement cheating
  }

  def receiveAllocations(turnAlloc: Double) = {
    facts = facts.update_allocated_appropriated(turnAlloc, false, None) //TODO: implement cheating
  }

//  def updateOpinions() = {
//    claims = claims.updateClaims(facts)
//    //biuld trust
//    //propagate trust
//  }
}


