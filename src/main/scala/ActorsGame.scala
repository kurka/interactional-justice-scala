import Network.Net
import Network.Net.StartTurn
import akka.actor.{Actor, ActorRef, ActorSystem, Inbox, PoisonPill, Props}

import scala.concurrent.duration._


object ActorsGame extends App {
  val rndSeed = 13
//  val net = new Net(10, 2, 0, rndSeed=rndSeed)
  val system = ActorSystem("lpgp")
  val net = system.actorOf(Net.props(10, 2, 0, rndSeed), "net")
  val inbox = Inbox.create(system)
  val nTurns = 1000

  println(net)

  //  (1 until nTurns).foreach(println(net.turn()))
  for (t <- 1 until nTurns) {
      inbox.send(net, StartTurn(t))
      println(inbox.receive(10.seconds))
  }
  net ! PoisonPill
//  system.stop(net)
  system.terminate()

}

//case object Greet
//case class WhoToGreet(who: String)
//case class Greeting(message: String)
//
//class Greeter extends Actor {
//  var greeting = ""
//
//  def receive = {
//    case WhoToGreet(who) => greeting = s"hello, $who"
//    case Greet           => sender ! Greeting(greeting) // Send the current greeting back to the sender
//  }
//}
//
//object ActorsGame extends App {
//
//  // Create the 'helloakka' actor system
//  val system = ActorSystem("helloakka")
//  // Create the 'greeter' actor
//  val greeter = system.actorOf(Props[Greeter], "greeter")
//  // Create an "actor-in-a-box"
//  val inbox = Inbox.create(system)
//  // Tell the 'greeter' to change its 'greeting' message
//  greeter.tell(WhoToGreet("akka"), ActorRef.noSender)
//
//  // Ask the 'greeter for the latest 'greeting'
//  // Reply should go to the "actor-in-a-box"
//  inbox.send(greeter, Greet)
//
//  // Wait 5 seconds for the reply with the 'greeting' message
//  val Greeting(message1) = inbox.receive(5.seconds)
//  println(s"Greeting: $message1")
//
//  // Change the greeting and ask for it again
//  greeter.tell(WhoToGreet("typesafe"), ActorRef.noSender)
//  inbox.send(greeter, Greet)
//  val Greeting(message2) = inbox.receive(5.seconds)
//  println(s"Greeting: $message2")
//
//  val greetPrinter = system.actorOf(Props[GreetPrinter])
//  // after zero seconds, send a Greet message every second to the greeter with a sender of the greetPrinter
//  system.scheduler.schedule(0.seconds, 1.second, greeter, Greet)(system.dispatcher, greetPrinter)
//
//}
//
//// prints a greeting
//class GreetPrinter extends Actor {
//  def receive = {
//    case Greeting(message) => println(message)
//  }
//}