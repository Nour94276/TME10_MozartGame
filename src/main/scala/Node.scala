package upmc.akka.leader

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy._

class NodeActor (val id : Int, val terminaux:List[Terminal]) extends Actor {
    val musicien = context.actorOf(Props(new Musicien(id,terminaux)), "Musicien"+id)
    val checker = context.actorOf(Props(new CheckerActor(id,self)), "Checker"+id)
    val heart = context.actorOf(Props(new HeartActor(id)), "Heart"+id)
    context.watch(musicien)
    context.watch(checker)
    context.watch(heart)
    context.system.scheduler.schedule(0 milliseconds, 200 milliseconds, checker, CheckStillAlive)
    context.system.scheduler.schedule(0 milliseconds, 200 milliseconds, checker, CheckDeadInside)

    def receive = { 
        case Start => musicien ! Start
        case FullDead(idMusicien) => musicien ! FullDead(idMusicien)
        case ClientConnected(idMusicien) => musicien ! ClientConnected(idMusicien)
        case Terminated(ref) =>
           println(s"Alerte: L'acteur ${ref.path.name} est mort.")
  }
}