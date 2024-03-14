package upmc.akka.leader

import akka.actor.{Props,  Actor,  ActorRef,  ActorSystem, ActorSelection}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
case class PlayConductor()
case class Maestro(id : Int)
case class Suicide()
case class Election()
case class Elu()
case class Start ()
case class GetMeasure(lancement : Int)
class Musicien (val id:Int, val terminaux:List[Terminal]) extends Actor {

 var musiciens = new Array[ActorSelection](4)
     val display = context.actorOf(Props[DisplayActor], name = "displayActor")
     val database = context.actorOf(Props[DataBaseActor], name = "database")
     var vivants = Array.fill[Boolean](4)(false)
     var idMaestro = -1
     def lancement : Int = {
            val r = new scala.util.Random
            val d1 = r.nextInt(5) + 1
            val d2 = r.nextInt(5) + 1
            d1 + d2
     }
     vivants(id) = true

          (0 to 3).foreach { i =>
          musiciens(i) = context.actorSelection(s"akka.tcp://MozartSystem$i@127.0.0.1:600$i/user/Node$i/Musicien$i")}
     if(idMaestro == id || idMaestro == -1) self ! Election  
     def receive: Receive = {
     case Start => display ! Message ("Le node " + this.id + " est dans la zone.")
     case Election => if(idMaestro == id|| idMaestro == -1) self ! Elu
     case Elu =>
            if(idMaestro != id) {
                idMaestro = id
                display ! Message ("Le Maestro est le node " + id)
                self ! PlayConductor
            }
      case PlayConductor => if(idMaestro == id) database ! GetMeasure (lancement)
     }
}
