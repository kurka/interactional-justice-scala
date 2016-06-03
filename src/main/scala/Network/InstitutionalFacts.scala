  package Network

  import Network.CheatMode._

  import scala.util.Random

  case class InstitutionalFacts(rnd: Random,
                                available:Double = 0.0,
                                provided:Double = 0.0,
                                needed:Double = 0.0,
                                demanded:Double = 0.0,
                                allocated:Double = 0.0,
                                appropriated:Double = 0.0){

    def update_available_demanded() = {
      val new_av = rnd.nextDouble()
      val new_need = new_av + rnd.nextDouble() * (1 - new_av)
      this.copy(available=new_av, needed=new_need)
    }

    def update_demand_provision(shouldCheat: Boolean, cheatOn: CheatMode) = {
      (shouldCheat, cheatOn) match {
        case (false, _) | (true, None) | (true, Appropriation) => this.copy(provided=this.available, demanded=this.needed)
        case (true, Provision) => this.copy(provided=rnd.nextDouble()*this.available, demanded = this.needed)
        case (true, Demand) => this.copy(provided=this.available, demanded=rnd.nextDouble()*(1-this.needed))
      }
    }

    def update_allocated_appropriated(alloc: Double, shouldCheat:Boolean, cheatOn:CheatMode) = {
      (shouldCheat, cheatOn) match {
        case (true, Appropriation) => this.copy(allocated=alloc, appropriated= alloc + rnd.nextDouble() * (1-alloc))
        case (_, _) =>  this.copy(allocated=alloc, appropriated=alloc)
      }
    }

  //  def update_available(new_value: Double) = this.copy(available=new_value)
  //  def update_provided(new_value: Double) = this.copy(provided=new_value)
  //  def update_needed(new_value: Double) = this.copy(needed=new_value)
  //  def update_demanded(new_value: Double) = this.copy(demanded=new_value)
  //  def update_allocated(new_value: Double) = this.copy(allocated=new_value)
  //  def update_appropriated(new_value: Double) = this.copy(appropriated=new_value)
  }