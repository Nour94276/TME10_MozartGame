package upmc.akka.leader

import akka.actor.{Props,  Actor,  ActorRef,  ActorSystem, ActorSelection}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global

case class CheckVivant()
case class CheckMort()
case class Vivant(id: Int)
case class Mort(id: Int)

class CheckerActor(val id : Int) extends Actor {
     override def preStart(): Unit = {
    context.system.scheduler.schedule(0.seconds, 200.milliseconds, self, CheckVivant)
    context.system.scheduler.schedule(0.seconds, 200.milliseconds, self, CheckMort)
  }
    var vivants = Array.fill[Boolean](4)(false)
    var cptNoSignal = new Array[Int](4)
    var hearts = Array.tabulate[ActorSelection](4) { i =>
    context.actorSelection(s"akka.tcp://MozartSystem$i@127.0.0.1:600$i/user/Node$i/Heart$i")}
    vivants(id) = true // Marquer soi-même comme vivant
       def receive: Receive = {
        case CheckVivant => {
            for(i <- 0 to 3)
                if (i != id) hearts(i) ! Check
        }
         case CheckMort => {
            for(i <- 0 to 3) {
                if (i != id){
                    cptNoSignal(i) = cptNoSignal(i) + 1
                    if(cptNoSignal(i) == 5) {
                        vivants(i) = false
                         println("Node " + i + " est mort")
                    }
                }
            }
        }
  }

}
