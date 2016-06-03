package object Network {
  object CheatMode extends Enumeration {
    type CheatMode = Value
    val None, Provision, Demand, Appropriation = Value
  }
}
