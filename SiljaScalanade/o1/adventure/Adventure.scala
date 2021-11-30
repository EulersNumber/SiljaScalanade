package o1.adventure

/** The class `Adventure` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of "hard-coded" information which pertain to a very
  * specific adventure game that involves a small trip through a twisted forest. All newly created
  * instances of class `Adventure` are identical to each other. To create other kinds of adventure
  * games, you will need to modify or replace the source code of this class. */
class Adventure {

  /** The title of the adventure game. */
  val title = "Baltic Sea Appro"

  /** Areas */
  private val promenade = new Area("Promenade", "You are right in the middle of the promenade on floor 8.\n" +
    "Drunken students make loud noices around you.\nThe sea is rough.")
  private val karaoke = new Area("Karaoke Bar", "You are at the Karaoke Bar. You see someone singing, off-key - obviously.\n"
    + "The air smells like cigarette smoke and booze.\nYou ponder whether you'd finally have the guts to steal the stage.")
  private val casino = new Area("Casino", "You are at the Casino. You hear slot machines spinning and coins jingling.\n" +
    "The atmosphere brings out the little child in you.")
  private val taxfree = new Area("Tax-free", "You are at the Tax-free shop.\nYou get caught up by all the nice self-care products.\nThe large collection of cheap liquor also catches your attention.")
  private val buffet = new Area("Buffet restaurant", "You are at the all-you-can-eat Buffet.\n" +
    "The price is quite high but then again, there are almost endless options.\nYou start to get all kinds of food cravings.")
  private val cabin = new Area("Cabin", "Own sweet cabin! Now the only thing you need is a full stamp-passport.")
  private val destination = cabin

  /** The character that the player controls in the game. */
  val player = new Player(promenade)

  /** Neighbors */
  promenade.setNeighbors(Vector("north" -> karaoke, "east" -> buffet, "south" -> casino, "west" -> taxfree))
  karaoke.setNeighbors(Vector("north" -> karaoke, "east" -> buffet, "south" -> promenade, "west" -> taxfree))
  casino.setNeighbors(Vector("north" -> promenade, "east" -> buffet, "south" -> casino, "west" -> taxfree))
  taxfree.setNeighbors(Vector("north" -> karaoke, "east" -> promenade, "south" -> casino, "west" -> taxfree))
  buffet.setNeighbors(Vector("north" -> karaoke, "east" -> cabin, "south" -> casino, "west" -> karaoke))
  cabin.setNeighbors(Vector("west" -> buffet))

  /** Items */
  karaoke.addItem(new Item("microphone", "It's a solid wireless microphone. Looks like its already on and the volume is set at the max level.\n" +
    "People are nodding approvingly."))
  promenade.addItem(new Item("map", s"It's the checkpoint map of the Baltic Sea Appro!\n\n${this.mapStatus()}\n\nGood luck, sailor!"))
  taxfree.addItem(new Item("deodorant sample", "It is a small deodorant sample. The scent is actually quite nice."))

  /** Activities */
  karaoke.addActivity("sing")
  casino.addActivity("gamble")
  buffet.addActivity("eat")
  taxfree.addActivity("souveniers")

  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of turns that this Appro game allows before time runs out. */
  val timeLimit = 40

  def mapStatus() = {
    s"Casino: win a game of roulette (${if(this.player.hasWonRoulette) "Stamp" else "Blank"})\nTax-free: buy souverniers for friends and family (${if(this.player.hasBoughtSouveniers) "Stamp" else "Blank"}).\nKaraoke Bar: sing Ukko Nooa in karaoke (${if(this.player.hasSungKaraoke)"Stamp" else "Blank"})\nBuffet: have a feast at the buffet restaurant (${if(this.player.hasEatenBuffet) "Stamp" else "Blank"})"
  }

  /** Determines if the Appro is complete, that is, if the player has won. */
  def isComplete = this.player.location == this.destination && this.player.hasCompletedTasks

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver = this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = "You are on a student Appro on Silja Scalanade - the queen of the Baltic Sea.\n" +
    "Complete the Appro and then, find your way back to the cabin.\n\n" +
    "Better hurry, 'cause you still have to collect all the stamps, and you need to get some sleep before tomorrow's exam.\n\n" +
    "Start by collecting the Appro checkpoint map."

  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage = {
    if (this.isComplete)
      "Finally, you arrived at the cabin with a full stamp passport and a deserved shiny overall badge!\nYou stumble your way to the bed, smile, and close your eyes thinking what an Appro it has been.\nZzz..."
    else if (this.turnCount == this.timeLimit)
      "Oh no! Time's up. With severe sleep deprivation, you collapse and fail to ace the Baltic Sea Appro.\nGame over!"
    else  // game over due to player quitting
      "Quitter! No overall bade for you then."
  }

  /** Plays a turn by executing the given in-game command, such as "go west". Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  def playTurn(command: String) = {
    val action = new Action(command)
    val outcomeReport = action.execute(this.player)
    if (outcomeReport.isDefined) {
      this.turnCount += 1
    }
    outcomeReport.getOrElse("Unknown command: \"" + command + "\".")
  }

}




