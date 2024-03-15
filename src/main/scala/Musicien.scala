package upmc.akka.leader
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global

import akka.actor._
import PlayerActor._
import ProviderActor._
import DataBaseActor._

case class Start ()
case class Bandmaster(id : Int)
case class ElectBandmaster()
case class IsElected()
case class Leave()
case class Conductor()
case class updateStatus (id : Int, status : Int)
case class Quit()
case class IamTheLeader()
case class SendCurrentNote()
case class ReceiveCurrentNote()


class Musicien (val id:Int, val terminaux:List[Terminal]) extends Actor {


    // Les differents acteurs du systeme
    val displayActor = context.actorOf(Props[DisplayActor], name = "displayActor")
    val database = context.actorOf(Props[DataBaseActor], name = "database")
    val player = context.actorOf(Props[PlayerActor], name = "player")
    val heart = context.actorOf(Props(new HeartActor(id)), name = "heart")
    val checker = context.actorOf(Props(new CheckerActor(id,heart)), name = "checker")
    val provider = context.actorOf(Props(new ProviderActor(self)), name = "provider")
    val rand = new scala.util.Random
    var currentNote =  null

    var availableMusicians = new Array[Int](4)
    var allMusiciansActor = new Array[ActorSelection](4)
    var nbOfOthersMusiciansAlive = 0
    var iAmAlone = false
    var leader = -1
    var countMusiciansAbscence = 0

    availableMusicians(id) = 0 

    for(i <- 0 to 3) {
        if (i != id){
            allMusiciansActor(i) = context.actorSelection("akka.tcp://MozartSystem" + i + "@127.0.0.1:600" + i + "/user/Musicien" + i)
            availableMusicians(i) = -1
        }
    }


    def lancementDeD : Int = {
            val r = new scala.util.Random
            val d1 = r.nextInt(5) + 1
            val d2 = r.nextInt(5) + 1
            d1 + d2
    }
    def receive = {
        case Start => {
            displayActor ! Message ("Musicien " + this.id + " is created")
        }

        case updateStatus(n, status) => {
            val previouslyAlive = availableMusicians(n)
            availableMusicians(n) = status

            if (status == 0 && (previouslyAlive == -1)) {
                if (nbOfOthersMusiciansAlive == 0 && leader == id) self ! Conductor
                // Lorsqu'un musicien revient à la vie
                iAmAlone = false
                nbOfOthersMusiciansAlive += 1
                if (leader != -1) allMusiciansActor(n) ! Bandmaster(leader)

            } else if (status == -1 && (previouslyAlive == 0)) {
                // Lorsqu'un musicien meurt
                nbOfOthersMusiciansAlive -= 1
            }
            else if (status == -1 && (previouslyAlive == 1)) {
                // Lorsqu'un chef meurt
                nbOfOthersMusiciansAlive -= 1
                self ! ElectBandmaster()
            }
            else if (status == 1 && (previouslyAlive == -1)) {
                nbOfOthersMusiciansAlive += 1
                // Lorsqu'un musicien devient leader
                leader = n
            }
            else if (status == -1 && previouslyAlive == -1  && leader == -1){
                countMusiciansAbscence += 1
                if(countMusiciansAbscence == 4) {
                    self ! ElectBandmaster()
                }
            }
        }

        case SendCurrentNote => {}
        case ReceiveCurrentNote => {}

        case Bandmaster(idMusician) => {
            if (idMusician != this.id && availableMusicians(idMusician) == 0){
                leader = idMusician
            }
        }
        case ElectBandmaster() => {
            var someOneElected = false
            var i = 3
            while (i >= 0 && !someOneElected) {
                if (availableMusicians(i) == 0) {
                    someOneElected = true
                    if (i == id) {
                        self ! IsElected
                        heart ! IamTheLeader
                    }
                    else {
                        leader = i
                    }
                }
                i -= 1
            }
        }
        case Quit =>
            if(iAmAlone) {
                displayActor ! Message ("Miiiiiiiiiiince ,je m'en vais tout seul... :(")
                context.system.terminate()
                System.exit(0)
        }

        case IsElected => {
            if(leader != id) {
                leader = id
                displayActor ! Message ("Je suis le chef ! Je vais lancer la musique !")
                self ! Conductor
            }
        }
        case Conductor => 
            if(leader == id) 
                provider ! GetMeasure (lancementDeD)
        
        case GetMeasure(m) => {
            database ! GetMeasure(m)
        }
        case Measure(chordlist) => {
            if(leader == id) {
                if(nbOfOthersMusiciansAlive > 0) {
                    var musicien = -1
                    var randMusicien = rand.nextInt(nbOfOthersMusiciansAlive) + 1
                    for(i <- 0 to 3){    
                        if(i != id && availableMusicians(i) == 0){
                            randMusicien -= 1
                            if(randMusicien == 0) musicien = i
                        }
                    }
                    displayActor ! Message ("Je fais jouer le musicien " + musicien + " !")
                    allMusiciansActor(musicien) ! Measure (chordlist)
                    context.system.scheduler.scheduleOnce(1800 milliseconds, self, Conductor)
                } else {
                    iAmAlone = true;
                    displayActor ! Message ("Acun musicien en vie... :). Je m'en vais dans 30 secondes...")
                    context.system.scheduler.scheduleOnce(30000 milliseconds, self, Quit)
                }
            } else {
                displayActor ! Message ("Je suis en train de jouer la note" + chordlist)
                player ! Measure (chordlist)
            }
        }
        
    }
}
