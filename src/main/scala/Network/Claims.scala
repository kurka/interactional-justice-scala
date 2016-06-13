package Network

case class Claims(f1a: Double,
                  f1b: Double,
                  f1c: Double,
                  f2: Double,
                  f3: Double,
                  f4: Double,
                  f5: Double,
                  f6: Double
                 ) {
  val allClaims = List(this.f1a, this.f1b, this.f1c, this.f2, this.f3, this.f6)

  def getPhi = allClaims.sum / allClaims.length

  def legitimateClaims(facts: InstitutionalFacts, prevStats: Stats, head: Boolean,
                       alpha:Double = 0.1, beta:Double = 0.1, a:Double = 2, b:Double = 1, c:Double = 3) = {
    /*
        Given each agent's perception of its condition, different claims are
        elaborated, assessing the justice of the processes, from a individual
        perspective

        nomenclature:             claims:
        g - available             f1a - Average_allocation
        p - provided              f1b - Satisfaction
        q - needed                f1c - Positive_allocations
        d - demanded              f2 - Average_demands
        r - allocated             f3 - Average_provision (decreasing)
        r' - appropriated         f4 - Prosumer_count (decreasing)
                                  f5 - Social utility (decreasing)
                                  f6 - social_compliancy (decreasing)
    */
    assert(facts.available.isDefined && facts.provided.isDefined && facts.needed.isDefined &&
      facts.demanded.isDefined && facts.allocated.isDefined && facts.appropriated.isDefined)

    val turnsPlayed = prevStats.turnsPlayed + 1
    val totalAllocation = prevStats.totalAllocation + facts.allocated.get
    val accruedResources = facts.appropriated.get + (facts.available.get - facts.provided.get)
    // utility
    val utility = if (accruedResources >= facts.needed.get) {
      a * facts.needed.get + b * (accruedResources - facts.needed.get)
    } else {
      a * accruedResources - c * (facts.needed.get - accruedResources)
    }
    // satisfaction
    val satisfaction = if (facts.appropriated.get >= facts.demanded.get) {
      (1-alpha) * prevStats.satisfaction + alpha
    } else {
      (1-beta) * prevStats.satisfaction
    }

    // f1a - average allocation
    val f1a = totalAllocation / turnsPlayed

    // f1b - satisfaction
    val f1b = satisfaction

    // f1c - positive allocations
    val posAlloc = if (facts.allocated.get > 0) 1 else 0
    val f1c = updateMean(this.f1c, turnsPlayed, posAlloc)

    // f2 - average demands
    val f2 = updateMean(this.f2, turnsPlayed, facts.demanded.get)

    // f3 - average provision (decreasing)
    val f3 = updateMean(this.f3, turnsPlayed, 1-facts.provided.get)

    // f4 - prosumer count (decreasing)
    val f4 = 1.0 //fixme?

    // f5 - Social utility (decreasing)
    val f5 = updateMean(this.f5, turnsPlayed, 1.0 - (if (head) 1.0 else 0.0))

    // f6 - social_compliancy (decreasing)
    val compliant = if (math.abs(facts.appropriated.get - facts.allocated.get) < 1e-5 &&
      math.abs(facts.provided.get - facts.available.get) < 1e-5 &&
      math.abs(facts.demanded.get - facts.needed.get) < 1e-5) 1.0 else 0.0

    val f6 = updateMean(this.f6, turnsPlayed, 1.0-compliant)

    val newStats = Stats(turnsPlayed, totalAllocation, satisfaction, utility)
    val newClaims = Claims(f1a, f1b, f1c, f2, f3, f4, f5, f6)
    (newStats, newClaims)
  }

  def updateMean(oldMean: Double, n: Int, newElem: Double) = {
    assert(n > 0)
    ((n-1)/n) * oldMean + (1/n) * newElem
  }

  def accordance(other: Claims) =
    (this.allClaims, other.allClaims).zipped.map({
      (mc, oc) => logistic(math.abs(mc - oc), k=15, thresh=0.25)
    }).sum

  def logistic(x: Double, k:Double = 15, thresh: Double = 0.25) =
    1 - (1 / (1 + math.exp(-k*(x-thresh))))

  def +(other: Claims) = Claims(
    this.f1a+other.f1a,
    this.f1b+other.f1b,
    this.f1c+other.f1c,
    this.f2+other.f2,
    this.f3+other.f3,
    this.f4+other.f4,
    this.f5+other.f5,
    this.f6+other.f6
  )

  def -(other: Claims) = Claims(
    this.f1a-other.f1a,
    this.f1b-other.f1b,
    this.f1c-other.f1c,
    this.f2-other.f2,
    this.f3-other.f3,
    this.f4-other.f4,
    this.f5-other.f5,
    this.f6-other.f6
  )

  def /(dividend: Double) = Claims(
    this.f1a/dividend,
    this.f1b/dividend,
    this.f1c/dividend,
    this.f2/dividend,
    this.f3/dividend,
    this.f4/dividend,
    this.f5/dividend,
    this.f6/dividend
  )
}
