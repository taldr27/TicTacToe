import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.util.Random

var board = ArrayBuffer[ArrayBuffer[String]]()

@main
def TicTacToe() =
  for (i <- 0 to 2)
    val row = ArrayBuffer[String]()
    for (j <- 0 to 2)
      row.addOne("")
    board.addOne(row)

  printBoard()

  var continueGame = true

  while (continueGame)
    println("Please enter a position (e.g 1, 1)")
    val input = StdIn.readLine()
    var x = 0
    var y = 0
    try
      val positions = input.split(",")
      x = positions(0).trim.toInt
      y = positions(1).trim.toInt

      var skipRound = false

      if (board(x-1)(y-1) != "")
        println("That position is already taken, try again")
        skipRound = true
      else
        board(x-1)(y-1) = "X"
        printBoard()

      if (!skipRound)
        val playerWon = checkWinner(true)
        if (playerWon)
          println("\uD83C\uDF7E \uD83C\uDF7E \uD83C\uDF7E \uD83C\uDF7E \uD83C\uDF7E")
          println("You won, congratulations!")
          continueGame = false

        val boardFull = checkBoardFull()
        if (boardFull && !playerWon)
          println("It's a tie")
          continueGame = false

        if (continueGame)
          placeComputerMove()
          printBoard()
          val computerWon = checkWinner(false)
          if (computerWon)
            println("Computer won")
            continueGame = false
    catch
      case e: Exception => println("Invalid input, please try again")

def printBoard() =
  println("-------------")
  for (i <- 0 to 2)
    for (j <- 0 to 2)
      board(i)(j) match
        case "X" => print("| X ")
        case "O" => print("| O ")
        case _ => print("|   ")
    println("|")
  println("-------------")

def randPos() = Random.nextInt(3)

def placeComputerMove() =
  var i = randPos()
  var j = randPos()
  while (board(i)(j) != "")
    i = randPos()
    j = randPos()
  board(i)(j) = "O"

def checkWinner(player: Boolean): Boolean =
  var won = false
  val checkSymbol = if (player) "X" else "O"

  for (i <- 0 to 2)
    // Horizontal wins
    if (board(i)(0) == checkSymbol && board(i)(1) == checkSymbol && board(i)(2) == checkSymbol)
      won = true

    // Vertical wins
    if (board(0)(i) == checkSymbol && board(1)(i) == checkSymbol && board(2)(i) == checkSymbol)
      won = true

    // Diagonal wins
    if (board(0)(0) == checkSymbol && board(1)(1) == checkSymbol && board(2)(2) == checkSymbol)
      won = true

    if (board(2)(0) == checkSymbol && board(1)(1) == checkSymbol && board(0)(2) == checkSymbol)
      won = true

  won

def checkBoardFull(): Boolean =
  var boardFull = true
  for (i <- 0 to 2)
    for (j <- 0 to 2)
      if (board(i)(j) == "")
        boardFull = false

  boardFull