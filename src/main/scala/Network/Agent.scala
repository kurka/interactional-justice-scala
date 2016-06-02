package Network



case class Agent(id: Int, neighbours: Set[Int],
                 //stats
                 turns_played:Int = 0,
                 total_allocation:Double = 0.0,
                 //attributes
                 reputation:Double = 0.0,
                 cheater:Boolean = false,
                 head:Boolean = false,
                 satisfaction:Double = 0.5,
                 utility:Double = 0.0) {
  val facts = InstitutionalFacts()
  val claims = Claims()
  def phi = claims.getPhi

  def setHead(set:Boolean = true) = this.copy(head=set)
}


