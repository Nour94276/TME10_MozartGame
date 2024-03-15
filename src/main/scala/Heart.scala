package upmc.akka.leader

import akka.actor.{Props,  Actor,  ActorRef,  ActorSystem, ActorSelection}


class HeartActor(val id : Int) extends Actor {
    var leader = false

    def receive = {
        case Check => {
            sender ! IamAlive(id, leader)
        }
        case IamTheLeader => {
            leader = true
        }
    }
}