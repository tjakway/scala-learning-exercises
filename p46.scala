//Truth Tables for logical expressions

object BooleanFunctions
{
  def and(a: Boolean, b: Boolean) = a && b
  def or(a: Boolean, b: Boolean) = a || b
  def not(a: Boolean) = !a
  //todo: implement using compose
  def nand(a: Boolean, b: Boolean) = not(and(a, b))
  def xor(a: Boolean, b: Boolean) = a != b
  //todo: what is impl?
  def equ(a: Boolean, b: Boolean) = a == b

  def getFunctions: List[(Boolean, Boolean) => Boolean] =
    List(and, or, nand, xor, equ)
}

case class Row(left: Boolean, right: Boolean, result: Boolean)

object TableFunctions
{
  type Table = List[Row]
  val boolean_domain = List(true, false)
  def table2(op: (Boolean, Boolean) => Boolean): Table = {
    boolean_domain.map((a: Boolean) => {
      boolean_domain.map((b: Boolean) => {
        Row(a, b, op(a, b)) 
      })
    }).flatten
    // ^ produces a List[List[Row]]
    // need to flatten it into a List[Row], AKA a Table
  }

  def tableToString(tab: Table): String = {
    val header = "A\tB\tresult"
    val rowStrings = tab.map(x => x match {
      case Row(a, b, r) => s"$a\t$b\t$r\n"
    })
    header + "\n" + rowStrings
  }
}

object p46 
{
  def main(args: Array[String]): Unit = {
    //see http://stackoverflow.com/questions/2886446/how-to-get-methods-list-in-scala
    val tables = BooleanFunctions.getFunctions.map(TableFunctions.table2(_))
    val tableStrings = tables.map(TableFunctions.tableToString(_) + "\n")
    tableStrings.foreach(println(_))
  }
}
