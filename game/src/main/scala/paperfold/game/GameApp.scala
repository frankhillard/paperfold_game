package paperfold.game

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.timers.setTimeout
import scala.util.{Failure, Success}

import org.scalajs.dom

import paperfold.{Folder, Instruction, Paper, UnfolderIA}

object GameApp:
  private var model: Model = Model.Menu
  private var token: Int = 0
  private var catalog: Vector[Level.Group] = Vector.empty
  private var activeSize: Option[String] = None
  private var catalogError: Option[String] = None
  private var solutionQueue: List[Instruction] = Nil

  def main(args: Array[String]): Unit =
    if dom.document.getElementById("app") != null then
      render()
      discoverLevels()

  private def actions: View.Actions =
    View.Actions(
      start = () =>
        ordered.headOption match
          case Some(entry) => loadEntry(entry)
          case None        => update(Model.LoadError(catalogError.getOrElse("No levels found.")))
      ,
      showHowTo = () =>
        token += 1
        update(Model.HowTo)
      ,
      backToMenu = () =>
        token += 1
        cancelSolution()
        update(Model.Menu)
      ,
      selectSize = selectSize,
      chooseLevel = loadEntry,
      selectCrease = selectCrease,
      fold = fold,
      retry = retry,
      nextLevel = nextLevel,
      solve = solve
    )

  private def update(next: Model): Unit =
    model = next
    render()

  private def render(): Unit =
    val root = dom.document.getElementById("app")
    root.innerHTML = ""
    root.appendChild(View.render(model, catalog, activeSize, catalogError, actions))

  /** Read every size folder under levels/ and keep the tabs in step. */
  private def discoverLevels(): Unit =
    Level.catalog().onComplete {
      case Success(groups) =>
        catalog = groups
        catalogError = None
        if activeSize.forall(size => !groups.exists(_.size == size)) then
          activeSize = groups.headOption.map(_.size)
        refreshQuietly()
      case Failure(err) =>
        catalogError = Some(messageOf(err))
        refreshQuietly()
    }

  private def ordered: Vector[Level.Entry] =
    catalog.flatMap(_.entries)

  private def selectSize(size: String): Unit =
    activeSize = Some(size)
    render()

  private def refreshQuietly(): Unit =
    val playing = model match
      case play: Model.Play => play.animating.nonEmpty || play.pendingNext
      case _                => false
    if !playing then render()

  private def loadEntry(entry: Level.Entry): Unit =
    cancelSolution()
    activeSize = Some(entry.size)
    update(Model.Loading(entry.level.level))
    takeLevel(entry)(
      onMissing = () => update(Model.LoadError(s"No level file at ${entry.path}")),
      onReady = level => openLevel(entry, level)
    )

  private def nextLevel(): Unit =
    model match
      case play: Model.Play if play.outcome == Outcome.Cleared && !play.pendingNext =>
        val following = ordered.dropWhile(_.path != play.path).drop(1).headOption
        following match
          case Some(entry) =>
            update(play.copy(pendingNext = true, note = None))
            loadEntry(entry)
          case None =>
            update(play.copy(pendingNext = false, note = Some("That was the last level.")))
      case _ => ()

  private def takeLevel(entry: Level.Entry)(onMissing: () => Unit, onReady: Level.Def => Unit): Unit =
    token += 1
    val current = token
    Level.load(entry.path).onComplete {
      case Success(Some(level)) if current == token => onReady(level)
      case Success(None) if current == token        => onMissing()
      case Failure(err) if current == token =>
        model match
          case play: Model.Play =>
            update(play.copy(pendingNext = false, note = Some(messageOf(err))))
          case _ =>
            update(Model.LoadError(messageOf(err)))
      case _ => ()
    }

  private def openLevel(entry: Level.Entry, level: Level.Def): Unit =
    activeSize = Some(entry.size)
    val number = level.level.toIntOption.getOrElse(0)
    try
      val paper = Paper.create(level.height, level.width, level.alphabet)
      update(
        Model.Play(
          levelNumber = number,
          path = entry.path,
          level = level,
          paper = paper,
          moves = Nil,
          crease = None,
          outcome = Outcome.Playing,
          animating = None,
          pendingNext = false,
          note = None
        )
      )
    catch
      case e: IllegalArgumentException =>
        update(Model.LoadError(messageOf(e)))

  private def selectCrease(crease: Crease): Unit =
    model match
      case play: Model.Play if play.solving => ()
      case play: Model.Play if play.outcome == Outcome.Playing && play.animating.isEmpty =>
        val next = if play.crease.contains(crease) then None else Some(crease)
        update(play.copy(crease = next))
      case _ => ()

  private def fold(instruction: Instruction): Unit =
    model match
      case play: Model.Play if !play.solving => beginFold(play, instruction)
      case _                                 => ()

  private def beginFold(play: Model.Play, instruction: Instruction): Unit =
    if play.animating.isEmpty && play.outcome == Outcome.Playing then
      token += 1
      val current = token
      update(play.copy(animating = Some(instruction), note = None))
      setTimeout(foldDelay()) {
        if current == token then
          val folded = Folder.fold(play.paper, instruction)
          val outcome =
            if Paper.height(folded) == 1 && Paper.width(folded) == 1 then
              if folded(0)(0) == play.level.target then Outcome.Cleared else Outcome.Failed
            else Outcome.Playing
          val stillSolving = solutionQueue.nonEmpty
          update(
            play.copy(
              paper = folded,
              moves = play.moves :+ instruction,
              crease = None,
              outcome = outcome,
              animating = None,
              pendingNext = false,
              note = None,
              solving = stillSolving
            )
          )
          if stillSolving then advanceSolution()
      }

  private def solve(): Unit =
    model match
      case play: Model.Play if play.animating.isEmpty && !play.solving && !play.pendingNext =>
        token += 1
        val current = token
        update(play.copy(note = None, solving = true, crease = None))
        setTimeout(0) {
          if current == token then
            try
              val started = dom.window.performance.now()
              val instructions =
                UnfolderIA.instructionsFor(play.level.height, play.level.width, play.level.target)
              val seconds = (dom.window.performance.now() - started) / 1000.0
              model match
                case latest: Model.Play if latest.path == play.path =>
                  val paper = Paper.create(play.level.height, play.level.width, play.level.alphabet)
                  solutionQueue = instructions
                  update(
                    latest.copy(
                      paper = paper,
                      moves = Nil,
                      crease = None,
                      outcome = Outcome.Playing,
                      animating = None,
                      pendingNext = false,
                      note = None,
                      solving = instructions.nonEmpty,
                      solutionSeconds = Some(seconds)
                    )
                  )
                  advanceSolution()
                case _ =>
                  cancelSolution()
            catch
              case e: IllegalArgumentException =>
                cancelSolution()
                model match
                  case latest: Model.Play => update(latest.copy(solving = false, note = Some(messageOf(e))))
                  case _                  => ()
        }
      case _ => ()

  private def advanceSolution(): Unit =
    solutionQueue match
      case Nil =>
        model match
          case play: Model.Play => update(play.copy(solving = false))
          case _                => ()
      case instruction :: rest =>
        solutionQueue = rest
        model match
          case play: Model.Play => beginFold(play, instruction)
          case _                => cancelSolution()

  private def cancelSolution(): Unit =
    solutionQueue = Nil

  private def retry(): Unit =
    model match
      case play: Model.Play =>
        token += 1
        cancelSolution()
        try
          val paper = Paper.create(play.level.height, play.level.width, play.level.alphabet)
          update(
            play.copy(
              paper = paper,
              moves = Nil,
              crease = None,
              outcome = Outcome.Playing,
              animating = None,
              pendingNext = false,
              note = None,
              solving = false,
              solutionSeconds = None
            )
          )
        catch
          case e: IllegalArgumentException =>
            update(Model.LoadError(messageOf(e)))
      case _ => ()

  private def foldDelay(): Double =
    if dom.window.matchMedia("(prefers-reduced-motion: reduce)").matches then 16
    else 520

  private def messageOf(err: Throwable): String =
    Option(err.getMessage).map(_.trim).filter(_.nonEmpty).getOrElse("Could not load that level.")
