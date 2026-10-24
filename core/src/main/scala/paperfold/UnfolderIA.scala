package paperfold

/**
 * Recovers a crease list by recursive IDA* (iterative deepening) on the fold tree.
 *
 * Unlike [[Unfolder]] (BFS of every fold) and [[UnfolderSmart]] (genetic search),
 * each stacked cell is required to appear in the target as a contiguous block,
 * either as-is or reversed. Folding only concatenates and reverses whole stacks,
 * so that invariant is exact and cuts most of the brute-force tree.
 */
object UnfolderIA:
  def instructionsFor(target: String): (List[Instruction], (Int, Int)) =
    require(target.nonEmpty, "cannot unfold an empty paper")
    val shapes = sortShapes(enqueueShapes(target.length, 1, Nil), Nil)
    val (found, nbfoldcalls) = searchShapes(shapes, target, 0)
    found match
      case Some(result) =>
        println(s"UnfolderIA target " + target + ", nbfoldcalls: " + nbfoldcalls)
        result
      case None => throw new IllegalArgumentException(s"cannot unfold $target")

  /** Folds that turn a blank `rows` by `cols` sheet into `target`. */
  def instructionsFor(rows: Int, cols: Int, target: String): List[Instruction] =
    require(target.nonEmpty, "cannot unfold an empty paper")
    require(rows > 0 && cols > 0 && rows * cols == target.length, "sheet does not match the target length")
    val paper = Paper.create(rows, cols)
    val (found, nbfoldcalls) = deepen(paper, target, minFoldsLeft(paper), rows + cols - 2, 0)
    found match
      case Some(insts) =>
        println(s"UnfolderIA target " + target + ", nbfoldcalls: " + nbfoldcalls)
        insts
      case None => throw new IllegalArgumentException(s"cannot unfold $target")

  def show(instructions: List[Instruction]): String =
    instructions match
      case Nil => ""
      case Instruction(part, index) :: Nil =>
        s"$part $index"
      case Instruction(part, index) :: tail =>
        s"$part $index\n" + show(tail)

  private def searchShapes(
      shapes: List[Array[Array[String]]],
      target: String,
      nbfoldcalls: Int
  ): (Option[(List[Instruction], (Int, Int))], Int) =
    shapes match
      case Nil => (None, nbfoldcalls)
      case paper :: rest =>
        val h = Paper.height(paper)
        val w = Paper.width(paper)
        val (found, cost) = deepen(paper, target, minFoldsLeft(paper), h + w - 2, nbfoldcalls)
        found match
          case Some(insts) => (Some((insts, (h, w))), cost)
          case None        => searchShapes(rest, target, cost)

  /** Raise the depth cap until a solution appears or the complete bound is exceeded. */
  private def deepen(
      paper: Array[Array[String]],
      target: String,
      limit: Int,
      maxLimit: Int,
      nbfoldcalls: Int
  ): (Option[List[Instruction]], Int) =
    if limit > maxLimit then (None, nbfoldcalls)
    else
      val (found, cost) = search(paper, target, Nil, 0, limit, Set.empty, nbfoldcalls)
      found match
        case Some(insts) => (Some(insts), cost)
        case None        => deepen(paper, target, limit + 1, maxLimit, cost)

  private def search(
      paper: Array[Array[String]],
      target: String,
      revSteps: List[Instruction],
      depth: Int,
      limit: Int,
      seen: Set[String],
      nbfoldcalls: Int
  ): (Option[List[Instruction]], Int) =
    val key = snapshot(paper)
    if seen.contains(key) then (None, nbfoldcalls)
    else if isSolved(paper, target) then (Some(reverse(revSteps)), nbfoldcalls)
    else if Paper.height(paper) == 1 && Paper.width(paper) == 1 then (None, nbfoldcalls)
    else if depth + minFoldsLeft(paper) > limit then (None, nbfoldcalls)
    else
      val (ops, cost) = nextPapers(paper, target, nbfoldcalls)
      tryOps(ops, target, revSteps, depth, limit, seen + key, cost)

  private def tryOps(
      ops: List[(Array[Array[String]], Instruction)],
      target: String,
      revSteps: List[Instruction],
      depth: Int,
      limit: Int,
      seen: Set[String],
      nbfoldcalls: Int
  ): (Option[List[Instruction]], Int) =
    ops match
      case Nil => (None, nbfoldcalls)
      case (folded, inst) :: rest =>
        if isSolved(folded, target) then (Some(reverse(inst :: revSteps)), nbfoldcalls)
        else
          val (found, cost) = search(folded, target, inst :: revSteps, depth + 1, limit, seen, nbfoldcalls)
          found match
            case Some(insts) => (Some(insts), cost)
            case None        => tryOps(rest, target, revSteps, depth, limit, seen, cost)

  /** Folds that keep every stack a (reversed) substring of the target, smallest sheet first. */
  private def nextPapers(
      paper: Array[Array[String]],
      target: String,
      nbfoldcalls: Int
  ): (List[(Array[Array[String]], Instruction)], Int) =
    def collect(
        ops: List[Instruction],
        cost: Int
    ): (List[(Int, Array[Array[String]], Instruction)], Int) =
      ops match
        case Nil => (Nil, cost)
        case head :: tail =>
          val folded = Folder.fold(paper, head)
          val (rest, restCost) = collect(tail, cost + 1)
          if compatible(folded, target) then
            ((area(folded), folded, head) :: rest, restCost)
          else (rest, restCost)
    val (items, cost) = collect(candidates(paper), nbfoldcalls)
    (sortByArea(items, Nil), cost)

  private def sortByArea(
      items: List[(Int, Array[Array[String]], Instruction)],
      acc: List[(Array[Array[String]], Instruction)]
  ): List[(Array[Array[String]], Instruction)] =
    items match
      case Nil => acc
      case head :: tail =>
        sortByArea(tail, insertByArea(head, acc))

  private def insertByArea(
      elt: (Int, Array[Array[String]], Instruction),
      sorted: List[(Array[Array[String]], Instruction)]
  ): List[(Array[Array[String]], Instruction)] =
    sorted match
      case Nil => List((elt._2, elt._3))
      case (paper, inst) :: rest =>
        if elt._1 <= area(paper) then (elt._2, elt._3) :: sorted
        else (paper, inst) :: insertByArea(elt, rest)

  private def compatible(paper: Array[Array[String]], target: String): Boolean =
    def at(row: Int, col: Int): Boolean =
      if row >= paper.length then true
      else if col >= paper(row).length then at(row + 1, 0)
      else
        val stack = paper(row)(col)
        (contains(target, stack) || contains(target, Folder.reverseStack(stack))) && at(row, col + 1)
    at(0, 0)

  private def contains(haystack: String, needle: String): Boolean =
    if needle.isEmpty then true
    else if haystack.length < needle.length then false
    else startsWith(haystack, needle) || contains(haystack.tail, needle)

  private def startsWith(haystack: String, needle: String): Boolean =
    if needle.isEmpty then true
    else if haystack.isEmpty then false
    else haystack.head == needle.head && startsWith(haystack.tail, needle.tail)

  private def isSolved(paper: Array[Array[String]], target: String): Boolean =
    Paper.height(paper) == 1 && Paper.width(paper) == 1 && paper(0)(0) == target

  private def minFoldsLeft(paper: Array[Array[String]]): Int =
    ceilLog2(Paper.width(paper)) + ceilLog2(Paper.height(paper))

  private def ceilLog2(n: Int): Int =
    if n <= 1 then 0
    else 1 + ceilLog2((n + 1) / 2)

  private def area(paper: Array[Array[String]]): Int =
    Paper.height(paper) * Paper.width(paper)

  private def enqueueShapes(
      n: Int,
      rows: Int,
      acc: List[Array[Array[String]]]
  ): List[Array[Array[String]]] =
    if rows > n then reverse(acc)
    else if n % rows == 0 then
      enqueueShapes(n, rows + 1, Paper.create(rows, n / rows) :: acc)
    else enqueueShapes(n, rows + 1, acc)

  /** Square-ish sheets first; a 2×2 is preferred over a 1×4 strip of the same area. */
  private def sortShapes(
      shapes: List[Array[Array[String]]],
      sorted: List[Array[Array[String]]]
  ): List[Array[Array[String]]] =
    shapes match
      case Nil          => sorted
      case head :: tail => sortShapes(tail, insertShape(head, sorted))

  private def insertShape(
      elt: Array[Array[String]],
      shapes: List[Array[Array[String]]]
  ): List[Array[Array[String]]] =
    shapes match
      case Nil => elt :: Nil
      case head :: tail =>
        if shapeBefore(elt, head) then elt :: shapes
        else head :: insertShape(elt, tail)

  private def shapeBefore(a: Array[Array[String]], b: Array[Array[String]]): Boolean =
    val ah = Paper.height(a)
    val aw = Paper.width(a)
    val bh = Paper.height(b)
    val bw = Paper.width(b)
    val aSquare = math.abs(ah - aw)
    val bSquare = math.abs(bh - bw)
    if aSquare != bSquare then aSquare < bSquare
    else if ah + aw != bh + bw then ah + aw < bh + bw
    else ah < bh

  private def candidates(paper: Array[Array[String]]): List[Instruction] =
    val cols = Paper.width(paper)
    val rows = Paper.height(paper)
    range(1, cols, 'L') ++ range(1, cols, 'R') ++ range(1, rows, 'T') ++ range(1, rows, 'B')

  private def range(from: Int, untilExclusive: Int, part: Char): List[Instruction] =
    if from >= untilExclusive then Nil
    else Instruction(part, from) :: range(from + 1, untilExclusive, part)

  private def snapshot(paper: Array[Array[String]]): String =
    def cells(row: Int, col: Int): String =
      if row >= paper.length then ""
      else if col >= paper(row).length then cells(row + 1, 0)
      else
        val sep = if col == 0 && row > 0 then ";" else if col > 0 then "," else ""
        sep + paper(row)(col) + cells(row, col + 1)
    s"${Paper.height(paper)}x${Paper.width(paper)}:" + cells(0, 0)

  private def reverse[A](list: List[A]): List[A] =
    def go(remaining: List[A], acc: List[A]): List[A] =
      remaining match
        case Nil          => acc
        case head :: tail => go(tail, head :: acc)
    go(list, Nil)
