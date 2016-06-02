package Network

case class InstitutionalFacts(available:Double = 0.0,
                              provided:Double = 0.0,
                              needed:Double = 0.0,
                              demanded:Double = 0.0,
                              allocated:Double = 0.0,
                              appropriated:Double = 0.0){

  def update_available(new_value: Double) = this.copy(available=new_value)
  def update_provided(new_value: Double) = this.copy(provided=new_value)
  def update_needed(new_value: Double) = this.copy(needed=new_value)
  def update_demanded(new_value: Double) = this.copy(demanded=new_value)
  def update_allocated(new_value: Double) = this.copy(allocated=new_value)
  def update_appropriated(new_value: Double) = this.copy(appropriated=new_value)
}