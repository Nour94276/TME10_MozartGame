package upmc.akka.leader

import akka.actor.{Actor, ActorRef, ActorSelection}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class CheckStillAlive()
case class CheckDeadInside(id: Int)
case class ClientConnected(id: Int)
case class FullDead(id: Int)
case class CheckChecker(id: Int)


class CheckerActor(val id: Int,node: ActorRef) extends Actor {
  implicit val ec = context.dispatcher
  val interval = 5000.milliseconds 
  var survivorsHeart = Array.fill[Boolean](4)(false)
  var newsurvivorsHeart = Array.fill[Boolean](4)(false)
  var hearts = new Array[ActorSelection](4)
  var  cptNoSignal = Array.fill[Int](4)(0)
     for(i <- 0 to 3) {
            try {
                hearts(i) = context.actorSelection("akka.tcp://MozartSystem" + i + "@127.0.0.1:600" + i + "/user/Node" + i + "/Heart" + i)
            } catch {
                case _ : Throwable => println("Erreur heart " + i)
            }

        if (i == id) 
        {survivorsHeart(i) = true}
        else {survivorsHeart(i) = false}
      newsurvivorsHeart = survivorsHeart
    }
  survivorsHeart(id) = true // Marquez soi-même comme vivant

  def receive: Receive = {
   case CheckStillAlive =>
      hearts.indices.foreach { i =>
               if (i != id && i>=0) {
          hearts(i) ! Check
          context.system.scheduler.scheduleOnce(interval) {
          node ! CheckDeadInside(i) 
          }
        }
      }
    case CheckDeadInside(idMusicien) =>
    {
         cptNoSignal(idMusicien) += 1
        if (cptNoSignal(idMusicien) == 5) {
          survivorsHeart(idMusicien) = false
          node ! FullDead(idMusicien)
        }

    }
    
    case CheckChecker(idMusicien) =>
      cptNoSignal(idMusicien) = 0
      survivorsHeart(idMusicien) = true 
      node ! ClientConnected(idMusicien)
}

  }
 

