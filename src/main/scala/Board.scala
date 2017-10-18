sealed trait BoardSquare
case class Block() extends BoardSquare
case class SnakePiece(n:Int) extends BoardSquare
case class Undecided() extends BoardSquare
case class Decided() extends BoardSquare


object SnakeBoard extends App {

  val maxWidth = 4
  val maxHeight = 4

var board = Array.ofDim[BoardSquare](maxHeight,maxWidth)
for(y <-board.indices){
  board(y).transform(x=> Undecided())
}

  board(0)(0) = Block()
  board(0)(2) = SnakePiece(1)
  board(1)(0) = SnakePiece(4)
  board(1)(2) = Block()
  board(2)(3) = SnakePiece(2)
  board(3)(0) = Block()
  board(3)(1) = SnakePiece(3)
  board(3)(3) = Block()


/*board.foreach { a =>
  a.foreach( bs => print(bs))
  print("\n")
}*/

  //SnakeUtils.allPossibleSnakes.foreach(println)
  SnakeUtils.repeatUntilDone(board)


}

object SnakeUtils {

  type Snake = List[(SnakePiece, Position)]
  type Position = (Int,Int)
  type Board = Array[Array[BoardSquare]]

  val maxWidth = 4
  val maxHeight = 4

  val allPossibleSnakes:List[List[(Int,Int)]] = for {
    x1 <- List((0,1), (1,0), (0,-1), (-1,0))
    x2 <- List((0,1), (1,0), (0,-1), (-1,0))
    x3 <- List((0,1), (1,0), (0,-1), (-1,0))
    if validSnakeVector(x1,x2,x3)
  } yield List(x1,x2,x3)

  def repeatUntilDone(board: Board) = {
    var start = divideAndConquer((List(), board))
    println(s"----first----")
    start.foreach(println)
    for(n<-(1 to 7)) {
      start = start.flatMap(sb => divideAndConquer(sb));
      println(s"----run $n-------")
      start.foreach(println)
      println(s"There are ${start.length} options")
    }
    start.foreach(println)
  }

  def validSnakeVector(x1:(Int,Int), x2:(Int,Int), x3:(Int,Int)):Boolean = List((x1,x2),(x2,x3)).forall {
    _ match {
      case ((0,1),(0,-1)) => false
      case ((0,-1),(0,1)) => false
      case ((-1,0),(1,0)) => false
      case ((1,0),(-1,0)) => false
      case _ => true
    }
  }

  def formSnakeFromVector(snakeVector: List[(Int, Int)], position: Position, n: Int):List[(SnakePiece,(Int,Int))] = {
    val forwardList:List[(SnakePiece, Position)] = snakeVector.drop(n-1).zipWithIndex.scanLeft((SnakePiece(n), position))(addPositions(true, _, _))
    val backwardList:List[(SnakePiece, Position)] = snakeVector.take(n-1).zipWithIndex.scanRight((SnakePiece(n), position))((next,last) => addPositions(forward=false, last, negativeVector(next)))

    //construct snake
    (backwardList ++ forwardList).distinct
  }

  def negativeVector(next:((Int,Int),Int)):((Int,Int),Int) = {
    next match {
      case ((x,y), n) => ((-x,-y),n)
    }
  }

  def addPositions(forward:Boolean, last:(SnakePiece, Position), next:((Int,Int),Int)):(SnakePiece, Position) = {
    val lastPiece = last._1
    val lastPosition = last._2
    val vector = next._1
    val index = next._2

    (if(forward){SnakePiece(lastPiece.n + 1)}else{SnakePiece(lastPiece.n - 1)}, (lastPosition._1 + vector._1, lastPosition._2 + vector._2))
  }

  def validSnake(snake: Snake, board:Array[Array[BoardSquare]]):Boolean = snake.forall{
    s => s._2._1 >= 0 && s._2._1 < maxHeight &&
      s._2._2 >= 0 &&  s._2._2 < maxWidth &&
      canPlacePiece(s._1, s._2, board)
  }

  def canPlacePiece(snakePiece: SnakePiece, position:Position, board: Board):Boolean = {
    if(position._1 < 0 || position._2 < 0){
      false
    }else {
      board(position._1)(position._2) match {
        case SnakePiece(n) => snakePiece.n == n
        case Undecided() => true
        case _ => false
      }
    }
  }

  def constructSnakes(snakePiece: SnakePiece, position:Position, board: Board):List[Snake] = for {
    snakeVector <- allPossibleSnakes
    snake = formSnakeFromVector(snakeVector, position, snakePiece.n)
    if validSnake(snake, board)
  } yield snake

  def zipWith2DArray[A](arr:Array[Array[A]]):Array[(A,(Int,Int))] = arr.map(_.zipWithIndex).zipWithIndex.flatMap( n => n._1.map( xs => (xs._1,(n._2,xs._2))) )

  def fillBoard(s: Snake, squareses: Board):Board = {
    s.foreach(xs => squareses(xs._2._1)(xs._2._2) = Decided())
    squareses
  }

  def divideAndConquer(snakesBoard:(Snake,Board)):List[(Snake,Board)] = {
    val origBoard = snakesBoard._2.clone()
    val unassignedSnakePieces = zipWith2DArray(snakesBoard._2).filter(_._1 match {
      case SnakePiece(_) => true
      case _ => false
    })
    if(unassignedSnakePieces.length > 0) {
      val possibleSnakes: List[Snake] = constructSnakes(unassignedSnakePieces.head._1.asInstanceOf[SnakePiece], unassignedSnakePieces.head._2, snakesBoard._2)
      possibleSnakes.map(s => ((snakesBoard._1 ++ s), fillBoard(s, origBoard.clone())))
    }else {
      List(snakesBoard)
    }
  }
}