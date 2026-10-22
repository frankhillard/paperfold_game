package paperfold

/**
 * Paper folding by merging rows or columns around a pivot index.
 *
 * The moving side is reflected across the crease, each moved stack is flipped
 * (string reversed), then laid on top of the stationary cell (prepended).
 */
object Folder:
  def applyAll(paper: Array[Array[String]], instructions: List[Instruction]): Array[Array[String]] =
    instructions match
      case Nil          => paper
      case head :: tail => applyAll(fold(paper, head), tail)

  def fold(paper: Array[Array[String]], instruction: Instruction): Array[Array[String]] =
    instruction.part match
      case 'L' => foldVertical(paper, instruction.index, leftOverRight = true)
      case 'R' => foldVertical(paper, instruction.index, leftOverRight = false)
      case 'T' =>
        transpose(foldVertical(transpose(paper), instruction.index, leftOverRight = true))
      case 'B' =>
        transpose(foldVertical(transpose(paper), instruction.index, leftOverRight = false))
      case other =>
        throw new IllegalArgumentException(s"Unknown fold part: $other")

  /** Reverse a stack so the former bottom layer becomes the top after a flip. */
  def reverseStack(stack: String): String =
    if stack.isEmpty then ""
    else reverseStack(stack.tail) + stack.head

  def transpose(paper: Array[Array[String]]): Array[Array[String]] =
    if paper.isEmpty then Array.empty
    else
      val rows = paper.length
      val cols = paper.head.length
      def column(c: Int, r: Int): Array[String] =
        if r >= rows then Array.empty
        else Array(paper(r)(c)) ++ column(c, r + 1)
      def columns(c: Int): Array[Array[String]] =
        if c >= cols then Array.empty
        else Array(column(c, 0)) ++ columns(c + 1)
      columns(0)

  private def foldVertical(
      paper: Array[Array[String]],
      pivot: Int,
      leftOverRight: Boolean
  ): Array[Array[String]] =
    require(paper.nonEmpty, "cannot fold an empty paper")
    val cols = Paper.width(paper)
    require(pivot >= 0 && pivot <= cols, s"column pivot $pivot out of range 0..$cols")
    def rows(r: Int): Array[Array[String]] =
      if r >= paper.length then Array.empty
      else
        val folded =
          if leftOverRight then foldLeftOverRight(paper(r), pivot)
          else foldRightOverLeft(paper(r), pivot)
        Array(folded) ++ rows(r + 1)
    rows(0)

  /** Left columns `[0, pivot)` flip onto the right columns `[pivot, n)`. */
  private def foldLeftOverRight(row: Array[String], pivot: Int): Array[String] =
    val leftLen = pivot
    val rightLen = row.length - pivot
    val width = math.max(leftLen, rightLen)
    def at(k: Int): String =
      val fromRight = if k < rightLen then row(pivot + k) else ""
      val fromLeft =
        if k < leftLen then reverseStack(row(pivot - 1 - k)) else ""
      fromLeft + fromRight
    def build(k: Int): Array[String] =
      if k >= width then Array.empty
      else Array(at(k)) ++ build(k + 1)
    build(0)

  /** Right columns `[pivot, n)` flip onto the left columns `[0, pivot)`. */
  private def foldRightOverLeft(row: Array[String], pivot: Int): Array[String] =
    val leftLen = pivot
    val rightLen = row.length - pivot
    val width = math.max(leftLen, rightLen)
    val pad = width - leftLen
    def at(k: Int): String =
      val leftIdx = k - pad
      val fromLeft =
        if leftIdx >= 0 && leftIdx < leftLen then row(leftIdx) else ""
      val j = width - 1 - k
      val fromRight =
        if j >= 0 && j < rightLen then reverseStack(row(pivot + j)) else ""
      fromRight + fromLeft
    def build(k: Int): Array[String] =
      if k >= width then Array.empty
      else Array(at(k)) ++ build(k + 1)
    build(0)
