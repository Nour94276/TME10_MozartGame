package upmc.akka.leader

import akka.actor.{Actor, ActorRef, ActorSelection}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.Props

case class CheckAllMusiciansStatus()
case class IamAlive(id : Int, leader : Boolean)
case class DisplayMusiciansStatus()
case class Check()

class CheckerActor(val id : Int, val heart : ActorRef) extends Actor {

    val displayActor = context.actorOf(Props[DisplayActor], name = "displayActor")
    val checkInterval = 900.milliseconds
    val maxNoSignal = 5
    
    override def preStart(): Unit = {
        context.system.scheduler.schedule(0.seconds, checkInterval, self, CheckAllMusiciansStatus)
        context.system.scheduler.schedule(0.seconds, 1000.milliseconds, self, DisplayMusiciansStatus)
    }
    var musiciansAlive = Array.fill[Int](4)(-1)
    var checkMaxSignal = new Array[Int](4)

    var musiciansHeartActor = Array.tabulate[ActorSelection](4) { i =>
        context.actorSelection(s"akka.tcp://MozartSystem$i@127.0.0.1:600$i/user/Musicien$i/heart")
    }
    musiciansAlive(id) = 0
    def receive: Receive = {
        case DisplayMusiciansStatus => {
            displayActor ! Message("Status des musiciens : ["+musiciansAlive.mkString(",") + "]")
        }
        case CheckAllMusiciansStatus => {
            (0 until 4).foreach { i =>
                if (i != id) {
                    musiciansHeartActor(i) ! Check // Envoyer un signal de vérification
                    checkMaxSignal(i) += 1
                    if (checkMaxSignal(i) == maxNoSignal) {
                        musiciansAlive(i) = -1
                        context.parent ! updateStatus(i, -1)
                        checkMaxSignal(i) = 0 // Réinitialiser pour éviter les répétitions
                    }
                }
                else {
                    heart ! Check
                }
            }
        }  
        case IamAlive (n, leader) => {
            checkMaxSignal(n) = 0
            musiciansAlive(n) = leader match {
                                        case true => 1
                                        case false => 0
                                    }
            context.parent ! updateStatus(n, musiciansAlive(n))
        }      
}

}
