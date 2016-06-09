type Square = Option[Int]

object Row
{
  //make sure we're only using 0-9
  def allInRange(nums: Seq[Int]) = {
    !nums.map(n => n >= 0 && n < 10).contains(false)
  }

  def noDuplicates(squares: Seq[Square]): Boolean = {
    squares.toSet.size == squares.size
  }
  
  def hasEmptySquares(squares: Seq[Square]) = {
    squares.map(x => x.isDefined).contains(false)
  }

  //solved means no empty squares and no duplicate numbers
  def isSolved(squares: Seq[Square]) = {
    !hasEmptySquares(squares) && noDuplicates(squares) 
      //just to be paranoid
      && allInRange(squares)
  }
}

object Grid
{
  def getGrid(topLevelGrid Seq[Seq[Square]], which: Int) = {
    val topLeft = (which * 3, which * 3)

  }
}

object SudokuSolver
{

  
  
}
