package paperfold

/** Recursively built grid of paper cells. Each cell is a stack of layer names (a String). */
object Paper:
  /** Hardcoded cell alphabet: one printable character per cell, in this order. */
  val alphabet: String =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$%&()*+,-./0123456789:;<=>?@[]^_{|}~"

  /** Row-major naming. Pass a level alphabet, or omit it to use Paper.alphabet. */
  def create(rows: Int, cols: Int, alphabet: String = Paper.alphabet): Array[Array[String]] =
    require(rows > 0 && cols > 0, "paper must have positive size")
    require(rows * cols <= alphabet.length, "paper is larger than the alphabet")
    def fillRow(row: Int, col: Int): Array[String] =
      if col >= cols then Array.empty
      else Array(cellName(row * cols + col, alphabet)) ++ fillRow(row, col + 1)
    def fill(row: Int): Array[Array[String]] =
      if row >= rows then Array.empty
      else Array(fillRow(row, 0)) ++ fill(row + 1)
    fill(0)

  def cellName(index: Int, alphabet: String = Paper.alphabet): String =
    require(index >= 0)
    def pick(remaining: String, n: Int): String =
      require(remaining.nonEmpty, "paper is larger than the alphabet")
      if n == 0 then remaining.head.toString
      else pick(remaining.tail, n - 1)
    pick(alphabet, index)

  def height(paper: Array[Array[String]]): Int = paper.length

  def width(paper: Array[Array[String]]): Int =
    if paper.isEmpty then 0 else paper.head.length

  def show(paper: Array[Array[String]]): String =
    def rowString(row: Int): String =
      if row >= paper.length then ""
      else
        val line = showRow(paper(row), 0)
        if row == paper.length - 1 then line
        else line + "\n" + rowString(row + 1)
    def showRow(row: Array[String], col: Int): String =
      if col >= row.length then ""
      else if col == row.length - 1 then row(col)
      else row(col) + " | " + showRow(row, col + 1)
    rowString(0)
