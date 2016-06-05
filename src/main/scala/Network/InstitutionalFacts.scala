  package Network

  import Network.CheatMode._

  import scala.util.Random

  case class InstitutionalFacts(rnd: Random,
                                available: Option[Double],
                                provided: Option[Double],
                                needed: Option[Double],
                                demanded: Option[Double],
                                allocated: Option[Double],
                                appropriated: Option[Double]
                               ){

    def updateAvailableDemanded() = {
      assert(available.isEmpty && provided.isEmpty && needed.isEmpty &&
        demanded.isEmpty && allocated.isEmpty && appropriated.isEmpty)
      val new_av = rnd.nextDouble()
      val new_need = new_av + rnd.nextDouble() * (1 - new_av)
      this.copy(available=Some(new_av), needed=Some(new_need))
    }

    def updateDemandProvision(shouldCheat: Boolean, cheatOn: CheatMode) = {
      assert(available.isDefined && needed.isDefined)
      (shouldCheat, cheatOn) match {
        case (false, _) | (true, NoCheat) | (true, Appropriation) => this.copy(provided=this.available, demanded=this.needed)
        case (true, Provision) => this.copy(provided=Some(rnd.nextDouble()*this.available.get), demanded = this.needed)
        case (true, Demand) => this.copy(provided=this.available, demanded=Some(rnd.nextDouble()*(1-this.needed.get)))
      }
    }

    def updateAllocatedAppropriated(alloc: Double, shouldCheat:Boolean, cheatOn:CheatMode) = {
      assert(available.isDefined && provided.isDefined && needed.isDefined && demanded.isDefined)
      (shouldCheat, cheatOn) match {
        case (true, Appropriation) => this.copy(allocated=Some(alloc), appropriated=Some(alloc + rnd.nextDouble() * (1-alloc)))
        case (_, _) =>  this.copy(allocated=Some(alloc), appropriated=Some(alloc))
      }
    }

  //  def update_available(new_value: Double) = this.copy(available=new_value)
  //  def update_provided(new_value: Double) = this.copy(provided=new_value)
  //  def update_needed(new_value: Double) = this.copy(needed=new_value)
  //  def update_demanded(new_value: Double) = this.copy(demanded=new_value)
  //  def update_allocated(new_value: Double) = this.copy(allocated=new_value)
  //  def update_appropriated(new_value: Double) = this.copy(appropriated=new_value)
  }