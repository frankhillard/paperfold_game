package paperfold.game

import paperfold.Instruction

enum Crease:
  case Column(pivot: Int)
  case Row(pivot: Int)

enum Outcome:
  case Playing
  case Failed
  case Cleared

enum Model:
  case Menu
  case HowTo
  case Loading(label: String)
  case LoadError(message: String)
  case Play(
      levelNumber: Int,
      path: String,
      level: Level.Def,
      paper: Array[Array[String]],
      moves: List[Instruction],
      crease: Option[Crease],
      outcome: Outcome,
      animating: Option[Instruction],
      pendingNext: Boolean,
      note: Option[String]
  )
