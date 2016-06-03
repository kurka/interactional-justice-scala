import Network.Net

import scala.util.Random


object game extends App {
  val rnd = new Random(13)
  val net = new Net(10, 2, rnd)
  net.assignRandomHead
  println("Hello World")
  println(net)

  val lpgpGame = new LPGP(net)
  lpgpGame.run(10)
}
