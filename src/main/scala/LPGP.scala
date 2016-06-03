import Network.Net

class LPGP(net: Net) {
  def run(nturns: Int) = 0 to nturns foreach {
    turn => {
//      net.updateAgentStatusAndRoles
      net.demandProvide()
      println(turn)
    }}
//  val net = Map
//  def round() = {
//    update_
//  }

}
