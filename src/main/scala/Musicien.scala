package upmc.akka.leader

import akka.actor.{Actor, ActorSelection, Props}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class PlayConductor()
case class Maestro(id : Int)
case class Suicide()
case class Election()
case class Elu()
case class Start ()
case class GetMeasure(lancement : Int)

class Musicien (val id:Int, val terminaux:List[Terminal]) extends Actor {
  var musiciens = new Array[ActorSelection](4)
  var survivors =new Array[Boolean](4)
  val display = context.actorOf(Props[DisplayActor], "displayActor")
  var idMaestro = -1
  var nbOthersAlive = 0;
  var secondMusicianAlive = false

 for(i <- 0 to 3) {
        if (i != id){
            musiciens(i) = context.actorSelection("akka.tcp://MozartSystem" + i + "@127.0.0.1:600" + i + "/user/Node" + i + "/Musicien" + i)
            survivors(i) = false
        }
    }
  def receive: Receive = {
    case Start =>
      println(s"Le node $id est vivant.")
    
    case ClientConnected(musicianId) =>
      if (!survivors(musicianId)) {
        survivors(musicianId) = true
        nbOthersAlive += 1
        println(s"Le musicien n° $musicianId est maintenant vivant.")
        println(s"Nombre de musiciens vivants : $nbOthersAlive")
      }
      
    case Election =>
      if(idMaestro == -1) {
        idMaestro = id
        println(s"Le Maestro est le node $id")
      }
    case FullDead(i) =>
    if(survivors(i))
    {
      println(s"Le node $id est mort.")
      survivors(i) = false
      nbOthersAlive -= 1
      println(s"Nombre de musiciens vivants : $nbOthersAlive")
      if (nbOthersAlive == 0) {
        println("Tous les musiciens sont morts.")
        context.system.terminate()
      }
    }
      
  }
}
