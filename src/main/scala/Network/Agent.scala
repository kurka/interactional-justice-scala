package Network

import scala.util.Random
import Network.CheatMode._


case class Agent(id: Int, var neighbours: Set[Int], rndSeed: Long,
                 //attributes
//                 var reputation:Double = 0.0,
                 var cheater:Boolean = false,
                 var head:Boolean = false,
                 var utility:Double = 0.0) {
  val rnd = new Random(rndSeed)
//  var facts = InstitutionalFacts(rnd)
  var claims = Claims(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  var stats = new Stats(turnsPlayed = 0, totalAllocation = 0.0, satisfaction = 0.5, utility = 0.0)
  def phi = claims.getPhi

  def new_turn(): InstitutionalFacts = {
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

  def updateOpinions(roundFacts: InstitutionalFacts) = {
    val (newStats, newClaims) = claims.legitimateClaims(roundFacts, stats, head)
    stats = newStats
    claims = newClaims
    stats
  }
}


