package upmc.akka.leader

import akka.actor._

case class Start ()
case class Bandmaster(id : Int)
case class ElectBandmaster()
case class IsElected()
case class Leave()


class Musicien (val id:Int, val terminaux:List[Terminal]) extends Actor {

    // Les differents acteurs du systeme
    val displayActor = context.actorOf(Props[DisplayActor], name = "displayActor")
    val database = context.actorOf(Props[DataBaseActor], name = "database")
    val player = context.actorOf(Props[PlayerActor], name = "player")

    var availableMusicians = new Array[Boolean](4)
    var allMusicians = new Array[ActorSelection](4)
    var leader = -1

    availableMusicians(id) = true 

    for(i <- 0 to 3) {
        if (i != id){
            allMusicians(i) = context.actorSelection("akka.tcp://MozartSystem" + i + "@127.0.0.1:600" + i + "/user/Node" + i + "/Musicien" + i)
            availableMusicians(i) = false
        }
    }

    def receive = {
        case Start => {
            displayActor ! Message ("Musicien " + this.id + " is created")
        }
        case Bandmaster(idMusician) => {
            if (idMusician != this.id && availableMusicians(idMusician)){
                leader = idMusician
            }
        }

        case ElectBandmaster() => {
            var someOneElected = false
            var i = 0
            while (i < 4 && !someOneElected) {
                if (availableMusicians(i)) {
                    someOneElected = true
                    if (i == id) 
                        self ! IsElected
                    else 
                        leader = i
                }
                i += 1
            }
        }

        case IsElected =>
            if(leader != id) {
                leader = id
                displayActor ! Message ("Musicien " + this.id + " est le leader")
                self ! player
            }
        
    }
}
