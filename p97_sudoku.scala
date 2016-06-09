sealed trait WhichSlot
case object First extends WhichSlot
case object Second extends WhichSlot
case object Third extends WhichSlot

sealed trait WhichDiagonal
case object Left extends WhichDiagonal
case object Right extends WhichDiagonal

abstract class Grid

//Line is a special case of Tuple3--it only has 1 component type
class Line[A] extends Tuple3[A, A, A]
{
  //map is well-defined for tuples of uniform type
  def map[B](f: (A) => B): Line[B] = {
    Line(f(_1), f(_2), f(_3))
  }
}

//every sudoku rectangle is a 3x3 grid and the sudoku grid itself is a 3x3 grid of squares
class Grid3[A](firstLine: Line[A], secondLine: Line[A], thirdLine: Line[A])
{
  type Line = (Int, Int, Int)

  def getRow(r: WhichSlot) = r match {
    case First => firstLine
    case Second => secondLine
    case Third => thirdLine
  }

  def getColumn(c: WhichSlot) = c match {
    //TODO: any way to rewrite accessor fields with functions?
    //i.e. instead have List(firstLine, secondLine, thirdLine) and case First => map (_1) , case Second => map (_2), etc.
    case First => firstLine._1, secondLine._1, thirdLine._1
    case Second => firstLine._2, secondLine._2, thirdLine._2
    case Third => firstLine._3, secondLine._3, thirdLine._3
  }

  //unfortunately tuple accessor fields are a lot of copying + pasting
  //I could just write a new function
  //def fst[A](tup: Tuple3[A]) = tup._1
  //but is there any more scala-like way to do this?
  def getDiagonal(d: WhichDiagonal) = d match {
    case Left => firstLine._1, secondLine._2, thirdLine._3
    case Right => firstLine._3, secondLine._2, thirdLine._1
  }
}

object SudokuSolver
{
  
}
