//import Network.Net
//import org.scalatest._
//
//import scala.util.Random
//
//class NetTest extends FlatSpec with Matchers {
//
//  "A Net" should "have just one head" in {
//    val net = new Net(10, 2, 0, 13)
//    val heads = net.agents.foldLeft(0)((acc, ag) => acc + (if (ag.head) 1 else 0))
//    heads should be (1)
//  }
//
//  it should "not have agents connect to themselves" in {
//    val net = new Net(10, 100, 3, 42)
//    net.agents.foreach(ag => ag.neighbours should not contain ag.id)
//  }
//
//}