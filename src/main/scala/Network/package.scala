package object Network {
  object CheatMode extends Enumeration {
    type CheatMode = Value
    val NoCheat, Provision, Demand, Appropriation = Value
  }
}
