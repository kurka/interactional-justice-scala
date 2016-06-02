//import Network.Net
//import org.scalatest._
//import org.scalatest.prop.PropertyChecks
//
//import scala.util.Random
//
//class NetProperties extends PropSpec with PropertyChecks with ShouldMatchers{
//  val rnd = new Random(17)
//  property("Nets should have right number of neighbours") {
//    forAll { (conns: Int) =>
//      whenever (conns > 0) {
//        val net = new Net(30, conns, rnd)
//        net.agents.foreach(ag => ag.neighbours should not contain ag.id)
//        net.agents.foreach(ag => ag.neighbours.size should be >= conns-1)
//        net.agents.foreach(ag => ag.neighbours.size should be <= net.agents.length-1)
//      }
//    }
//  }
//}