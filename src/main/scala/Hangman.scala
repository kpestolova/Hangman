import scala.io.StdIn.readLine
import scala.util.Random
import scala.io.Source

object Hangman extends App {

  println("Hey! You're playing Hangman!")
  var wins = 0
  var losses = 0
  Game


  def Game:Any = {

    /**
     * Extracts words from the file and filters them
     */

    val fName = Source.fromResource("Words.txt")
    val listOfWords = fName.getLines.toList
    var level = List("")
    val easyWords = listOfWords.filter(_.length < 5)
    val mediumWords = listOfWords.filter(x => x.length > 4 && x.length < 10)
    val hardWords = listOfWords.filter(_.length > 9)
    fName.close()


    val input = readLine("\nDo you have 1 player or 2? ")
    var word = ""
    var guesses  = 6
    val minGuesses = 0

    /**
     * Checks player quantity and gets the guessing word
     * Asks the difficulty level
     */

    input match {
      case "1"|"one"|"One" => val lvl = readLine("\nCool! Let's play together. Choose the difficulty level: [E]asy, [M]edium or [H]ard ").replaceAll("\\s", "")
        lvl match {
          case "E"|"e"|"easy"|"Easy" => level = easyWords
            guesses = 8
          case "M"|"m"|"medium"|"Medium" => level = mediumWords
          case "H"|"h"|"hard"|"Hard" => level = hardWords
          case _ => println("\nNext time write a letter! Try again")
            Game
        }
        word = Random.shuffle(level).head.toUpperCase
      case "2"|"Two"|"two" =>  println("\nOkay, nice to see you both! \n" +
        "Now one of you should close their eyes.")
        word = readLine("\nPlease enter your word: ").toUpperCase
        println("\n" *10)
      case _ => println("Sorry, I can't handle this. " +
        "Restart the programme and try again")
        Game
    }

    /**
     * Makes the word's structure for the game
     */

    val wordList = word.replaceAll("\\s", "").toList
    var lowerList = ("_" *  wordList.length).toList
    println(lowerList.mkString(" "))

    /**
     * Game process:
     * Counts guesses, used letters, wins and losses
     * Checks if the guessing word contains the input letter
     * Stops when player runs out of tries or when player guesses the word
     *
     */

    var usedLetters : Set[Char] = Set()
    while (guesses != minGuesses) {
      val inLetter: Char = readLine("\nType the letter: ").toUpperCase.head

      if (usedLetters.contains(inLetter)) {
        println(lowerList.mkString(" ")+"\nYou already used this letter!")
      }
      else if (wordList.contains(inLetter)) {
        lowerList = lowerList.zip(wordList).map({ case (l, w) => if (inLetter == w) w else l })
        println(lowerList.mkString(" ") + " "* 5 + " Guesses left: " + guesses)

        if (lowerList == wordList) {
          wins += 1
          println("\nYou win! Congrats!")
          guesses = minGuesses
        }
      } else {
        guesses -= 1
        println(lowerList.mkString(" ") + " "* 5 +" Guesses left: " + guesses)
        if (minGuesses == guesses) {
          losses += 1
          println("\nSorry, you lose :( \nThe word was: " + wordList.mkString(""))
        }
      }
      usedLetters += inLetter
      println(usedLetters.mkString(", "))
    }

    /**
     *Asks if player wants to play again
     *Depending on the input it starts a new game or stops
     */

    println("Wins: " +wins+" Losses: " + losses)
    val tryAgain = readLine("\nDo you want to try again? ")

    if (tryAgain.startsWith("Y") || tryAgain.startsWith("y")) {
      println("Then let's continue!")
      Game
    }
    else println("Okay, see you next time!")
  }
}



