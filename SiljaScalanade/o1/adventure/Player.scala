package o1.adventure
import scala.collection.mutable.Map
import o1.sound.midi.play
import o1.odds.Odds
import scala.util.Random

/** A `Player` object represents a player character controlled by the real-life user of the program.
  *
  * A player object's state is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(startingArea: Area) {

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private val items = Map[String, Item]()

  /** Alcohol-related variables */
  private var permilles = 0.0
  var beers = 15

  /** Player tasks */
  var hasWonRoulette = false
  var hasSungKaraoke = false
  var hasBoughtSouveniers = false
  var hasEatenBuffet = false

  /** Player is carrying beer */
  val beer = new Item("beer", "It's a pocket warm Sandels.")
  this.items += beer.name -> beer

  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven

  /** Determines whether player has completed all his tasks */
  def hasCompletedTasks = {
    this.hasWonRoulette && this.hasSungKaraoke && this.hasBoughtSouveniers && this.hasEatenBuffet
  }

  /** Returns the current location of the player. */
  def location = this.currentLocation


  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player's current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */
  def go(direction: String) = {
    val destination = this.location.neighbor(direction)
    this.currentLocation = destination.getOrElse(this.currentLocation)
    if (destination.isDefined) "You go " + direction + "." else "You can't go " + direction + "."
  }


  /** Causes the player to rest for a short while (this has no substantial effect in game terms).
    * Returns a description of what happened. */
  def rest() = {
    "You rest for a while. Better get a move on, though."
  }


  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() = {
    this.quitCommandGiven = true
    ""
  }

  def getHelp = {
    s"If you don't know what do do, just examine the map. It shows the checkpoints of the Appro, and tells you which checkopints/tasks are left. If you don't have them map, return to the promenade and pick it up from there.\n\nFunctions:\n- use [itemname]: uses an item, if it can be used in the first place.\n- [activity name]: engages in an activity (e.g., gamble or eat). These can only be done when the activity is available at some location.\n\nIf you need further help, read the walktrough document and/or view the map of Silja Scalanade."
  }

  def use(itemName: String): String = {
    if (this.has(itemName)) {
      val item = this.items(itemName)
      item.name match {
        case "beer" => this.drinkBeer()
        case "microphone" => """type 'sing' to use the microphone."""
        case "deodorant sample" => this.useDeo()
        case "map" => "type 'examine map' to view the checkpoints and corresponding tasks."
        case _ => "this item cannot be used. Instead, try the 'examine'-command or engage in an activity."
      }
    } else {
      "You don't have that!"
    }
  }

  def buySouveniers() = {
    this.hasBoughtSouveniers = true
    "You pay a 75€ for a collection of tax-free products:\n" +
    "- Ahlgren's Bilar. This one is a souvenier for your friend who is a koneteekkari and likes cars.\n" +
    "- a large Toblerone bar. This one is for your sweet sister.\n" +
    "- a bottle of fine cognac. This one is for your father."
  }

  def sing(): String = {
    if(this.currentLocation.name == "Karaoke Bar" && this.has("microphone")) {
      play("cccedddfeeddc---eeeeg-f-ddddf-e-cccedddfeeddc---")
      this.hasSungKaraoke = true
      "[Singing...]"
    } else {
      "The only place I dare to sing at is the Karaoke Bar, and I must have a mic."
    }
  }

  def drinkBeer(): String = {
    if (this.beers > 0) {
      this.beers -= 1
      this.permilles += 0.1
      "Glug, glug, glug, ahh... Well deserved beer, as always.\n" +
        "You feel fueled up again and more ready to continue the journey.\n\n" +
        "Current permilles: " + this.permilles.toString +
        "\nBeers left: " + this.beers.toString
    } else {
      "You are out of beers, unfortunately.\n\n" +
        "Current permilles: " + this.permilles.toString
    }
  }

  def useDeo(): String = {
    this.items -= "deodorant sample"
    "Pff, pff. You put some of the sample deodorant on.\nYou like the vanilla-like scent."
  }

  def eat(): String = {
    this.hasEatenBuffet = true
    "Your cheeks turn red as you pay 49€ for the buffet.\n" +
      "At least you're hungry and determined to get bang for your buck.\n" +
      "You choose to eat archipelago bread with skagen, smoked gravlax, and good ol' french fries and sausages.\n" +
      "The fiest is done, and you feel confident that this dinner will provide you the needed energy for the rest of the Appro."
  }

   /** Simulates a game of roulette. The player has three strategies, of which one is chosen randomly. */
  def playRoulette() = {

    // Roulette table
    val reds = Vector(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
    val blacks = Vector(2,4,6,8,10,11,13,15,17,20,22,24,26,28,29,31,33,35)

    // Set of betting strategies
    val strategies = Vector("single", "red", "black")

    val randomIndex = Random.nextInt(3) // used to select betting strategy
    val chosenStrat = strategies(randomIndex) // strategy name

    // Outcome of a single wheel spin
    val wheelOutcome = Random.nextInt(37)

    if (chosenStrat == "single") {
      val betNumber = Random.nextInt(37)
      if (betNumber == wheelOutcome) {
        this.hasWonRoulette = true
        s"You play a round of French style roulette.\nCongratulations, you won!\nThis time, you chose a single bet on ${betNumber}, and it turned out right."
      } else {
        s"You play a round of French style roulette.\nUnfortunately, you lost.\nThis time, you chose a single bet on ${betNumber}, but the ball fell into pocket number ${wheelOutcome}."
      }} else if (chosenStrat == "red") {
        if (reds.contains(wheelOutcome)) {
          this.hasWonRoulette = true
          s"You play a round of French style roulette.\nCongratulations, you won!\nThis time, you chose to bet on red and the ball fell into pocket number ${wheelOutcome}, which is red."
        } else if (blacks.contains(wheelOutcome)) {
          s"You play a round of French style roulette.\nUnfortunately, you lost.\nThis time, you chose to bet on red but the ball fell into pocket number ${wheelOutcome}, which is black."
        } else {
          s"You play a round of French style roulette.\nUnfortunately, you lost.\nThis time, you chose to bet on red but the ball fell into pocket number ${wheelOutcome}, which is green."
        }} else {
        if (reds.contains(wheelOutcome)) {
          s"You play a round of French style roulette.\nUnfortunately, you lost.\nThis time, you chose to bet on black but the ball fell into pocket number ${wheelOutcome}, which is red."
        } else if (blacks.contains(wheelOutcome)) {
          this.hasWonRoulette = true
          s"You play a round of French style roulette.\nCongratulations, you won!\nThis time, you chose to bet on black and the ball fell into pocket number ${wheelOutcome}, which is black."
        } else {
          s"You play a round of French style roulette.\nUnfortunately, you lost.\nThis time, you chose to bet on red but the ball fell into pocket number ${wheelOutcome}, which is green."
        }
      }
    }

  def drop(itemName: String): String = {
    if (this.has(itemName)) {
      val item = this.items(itemName)
      this.items -= itemName
      this.currentLocation.addItem(item)
      "You drop the " + itemName + "."
    } else {
      "You don't have that!"
    }
  }

  def mapStatus() = {
    s"Casino: win a game of roulette (${if(this.hasWonRoulette) "Stamp" else "Blank"})\nTax-free: buy souverniers for friends and family (${if(this.hasBoughtSouveniers) "Stamp" else "Blank"})\nKaraoke Bar: sing Ukko Nooa in karaoke (${if(this.hasSungKaraoke)"Stamp" else "Blank"})\nBuffet: have a feast at the buffet restaurant (${if(this.hasEatenBuffet) "Stamp" else "Blank"})"
  }

  def examine(itemName: String): String = {
    if (this.has(itemName)) {
      if (itemName == "map") {
        s"You look closely at the checkpoint map of the Baltic Sea Appro.\n(Stamp) means that you have completed a task, (Blank) means that the task is not completed.\n\n${this.mapStatus()}\n\nGood luck, sailor!"
      } else {
        s"You look closely at the $itemName.\n${this.items(itemName).description}"
      }
    } else {
       "If you want to examine something, you need to pick it up first."
    }
  }

  def has(itemName: String): Boolean = this.items.contains(itemName)

  def inventory: String = {
    if (this.items.nonEmpty) {
       "You are carrying:\n" + this.items.keys.mkString("\n")
    } else {
       "You are empty-handed."
    }
  }

  def get(itemName: String): String = {
    if (this.currentLocation.contains(itemName)) {
      val item = this.currentLocation.removeItem(itemName)
      this.items += itemName -> item.get
      item.get.name match {
        case _ => "You pick up the " + itemName + "."
      }
    } else {
      "There is no " + itemName + " here to pick up."
    }
  }

  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name

}


