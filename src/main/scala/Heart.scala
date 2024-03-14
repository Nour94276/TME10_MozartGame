package upmc.akka.leader

import akka.actor.{Props,  Actor,  ActorRef,  ActorSystem, ActorSelection}

case class Check()

class HeartActor(val id : Int) extends Actor {

     def receive: Receive = {
     case Check => sender ! Vivant(id)
     }
}