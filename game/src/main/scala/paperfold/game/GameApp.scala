package paperfold.game

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.timers.setTimeout
import scala.util.{Failure, Success}

import org.scalajs.dom

import paperfold.{Folder, Instruction, Paper}

object GameApp:
  private var model: Model = Model.Menu
  private var token: Int = 0
  private var catalog: Vector[Level.Entry] = Vector.empty

  def main(args: Array[String]): Unit =
    if dom.document.getElementById("app") != null then
      render()
      discoverLevels(1, Vector.empty)

  private def actions: View.Actions =
    View.Actions(
      start = () => loadLevel(1),
      showHowTo = () =>
        token += 1
        update(Model.HowTo)
      ,
      backToMenu = () =>
        token += 1
        update(Model.Menu)
      ,
      chooseLevel = loadLevel,
      selectCrease = selectCrease,
      fold = fold,
      retry = retry,
      nextLevel = nextLevel
    )

  private def update(next: Model): Unit =
    model = next
    render()

  private def render(): Unit =
    val root = dom.document.getElementById("app")
    root.innerHTML = ""
    root.appendChild(View.render(model, catalog, actions))

  /** Walk levels/levelN.json until a number is missing, and keep the menu list in step. */
  private def discoverLevels(n: Int, found: Vector[Level.Entry]): Unit =
    Level.load(n).onComplete {
      case Success(Some(level)) =>
        val next = found :+ Level.Entry(n, level)
        catalog = next
        refreshQuietly()
        discoverLevels(n + 1, next)
      case _ => ()
    }

  private def refreshQuietly(): Unit =
    val playing = model match
      case play: Model.Play => play.animating.nonEmpty || play.pendingNext
      case _                => false
    if !playing then render()

  private def loadLevel(n: Int): Unit =
    update(Model.Loading(n))
    takeLevel(n)(
      onMissing = () => update(Model.LoadError(s"No level file at levels/level$n.json")),
      onReady = level => openLevel(n, level)
    )

  private def nextLevel(): Unit =
    model match
      case play: Model.Play if play.outcome == Outcome.Cleared && !play.pendingNext =>
        update(play.copy(pendingNext = true, note = None))
        val n = play.levelNumber + 1
        takeLevel(n)(
          onMissing = () =>
            model match
              case current: Model.Play =>
                update(current.copy(pendingNext = false, note = Some("That was the last level.")))
              case _ => ()
          ,
          onReady = level => openLevel(n, level)
        )
      case _ => ()

  private def takeLevel(n: Int)(onMissing: () => Unit, onReady: Level.Def => Unit): Unit =
    token += 1
    val current = token
    Level.load(n).onComplete {
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

  private def openLevel(n: Int, level: Level.Def): Unit =
    try
      val paper = Paper.create(level.height, level.width, level.alphabet)
      update(
        Model.Play(
          levelNumber = n,
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
      case play: Model.Play if play.outcome == Outcome.Playing && play.animating.isEmpty =>
        val next = if play.crease.contains(crease) then None else Some(crease)
        update(play.copy(crease = next))
      case _ => ()

  private def fold(instruction: Instruction): Unit =
    model match
      case play: Model.Play => beginFold(play, instruction)
      case _                => ()

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
          update(
            play.copy(
              paper = folded,
              moves = play.moves :+ instruction,
              crease = None,
              outcome = outcome,
              animating = None,
              pendingNext = false,
              note = None
            )
          )
      }

  private def retry(): Unit =
    model match
      case play: Model.Play =>
        token += 1
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
              note = None
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
