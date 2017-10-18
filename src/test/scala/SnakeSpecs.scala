import org.scalatest.{FlatSpec, Matchers}


class SnakeSpecs extends FlatSpec with Matchers {
  val emptyBoard:Array[Array[BoardSquare]] = Array.ofDim[BoardSquare](4,4)
  for(y <-emptyBoard.indices){
    emptyBoard(y).transform(x=> Undecided())
  }

  "A valid snake" should "not go out of bounds - lowerbound - right" in {
    val snake = List((SnakePiece(1),(0,0)),(SnakePiece(2),(0,-1)))

    SnakeUtils.validSnake(snake, emptyBoard) shouldBe false
  }

  "A valid snake" should "not go out of bounds - lowerbound - left" in {
    val snake = List((SnakePiece(1),(0,0)),(SnakePiece(2),(-1,0)))

    SnakeUtils.validSnake(snake, emptyBoard) shouldBe false
  }

  //using default need dynamic test later
  "A valid snake" should "not go out of bounds - upperbound - right" in {
    val snake = List((SnakePiece(1),(3,3)),(SnakePiece(2),(3,4)))

    SnakeUtils.validSnake(snake, emptyBoard) shouldBe false
  }

  //using default need dynamic test later
  "A valid snake" should "not go out of bounds - upperbound - left" in {
    val snake = List((SnakePiece(1),(3,3)),(SnakePiece(2),(4,3)))

    SnakeUtils.validSnake(snake,emptyBoard) shouldBe false
  }

  "A valid snake" should "stay within bounds" in {
    val snake = List((SnakePiece(1),(0,0)),(SnakePiece(2),(0,1)))

    SnakeUtils.validSnake(snake, emptyBoard) shouldBe true
  }

  "A snake placed on a block" should "not be valid" in {
    val snake = List((SnakePiece(1),(0,0)),(SnakePiece(2),(0,1)))
    val board:Array[Array[BoardSquare]] = Array.ofDim[BoardSquare](2,2)
    for(y <-board.indices){
      board(y).transform(x=> Undecided())
    }
    board(0)(1) = Block()
    SnakeUtils.validSnake(snake, board) shouldBe false
  }

  "A snake placed on another snakepiece" should "not be valid" in {
    val snake = List((SnakePiece(1),(0,0)),(SnakePiece(2),(0,1)))
    val board:Array[Array[BoardSquare]] = Array.ofDim[BoardSquare](2,2)
    for(y <-board.indices){
      board(y).transform(x=> Undecided())
    }
    board(0)(1) = SnakePiece(3)
    SnakeUtils.validSnake(snake, board) shouldBe false
  }

  "A snake placed on the same snakepiece" should "be valid" in {
    val snake = List((SnakePiece(1),(0,0)),(SnakePiece(2),(0,1)))
    val board:Array[Array[BoardSquare]] = Array.ofDim[BoardSquare](2,2)
    for(y <-board.indices){
      board(y).transform(x=> Undecided())
    }
    board(0)(1) = SnakePiece(2)
    SnakeUtils.validSnake(snake, board) shouldBe true
  }

  "A snakevector with snakehead" should "be able to form a snake" in {
    val snake = List((SnakePiece(1),(0,0)),(SnakePiece(2),(0,1)))

    SnakeUtils.formSnakeFromVector(List((0,1)), (0,0), 1) shouldBe snake
  }

  "A snakevector with snaketail" should "be able to form a snake" in {
    val snake = List((SnakePiece(1),(0,0)),(SnakePiece(2),(0,1)))

    SnakeUtils.formSnakeFromVector(List((0,1)), (0,1), 2) shouldBe snake
  }

  "A snakevector with snakemiddle" should "be able to form a snake" in {
    val snake = List((SnakePiece(1),(0,0)),(SnakePiece(2),(0,1)),(SnakePiece(3),(1,1)))

    SnakeUtils.formSnakeFromVector(List((0,1),(1,0)), (0,1), 2) shouldBe snake
  }

}
