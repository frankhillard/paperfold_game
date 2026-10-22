package paperfold

/** A folding instruction: L/R fold around a column pivot, T/B around a row pivot. */
final case class Instruction(part: Char, index: Int)

object Instruction:
  def parse(part: String, index: Int): Instruction =
    val p = part.trim.toUpperCase
    require(p.length == 1 && "LRTB".contains(p.head), s"Unknown fold part: $part")
    Instruction(p.head, index)
