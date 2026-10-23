package paperfold.game

import org.scalajs.dom

import paperfold.{Instruction, Paper}

object View:
  final case class Actions(
      start: () => Unit,
      showHowTo: () => Unit,
      backToMenu: () => Unit,
      selectSize: String => Unit,
      chooseLevel: Level.Entry => Unit,
      selectCrease: Crease => Unit,
      fold: Instruction => Unit,
      retry: () => Unit,
      nextLevel: () => Unit
  )

  def render(
      model: Model,
      groups: Seq[Level.Group],
      activeSize: Option[String],
      catalogError: Option[String],
      actions: Actions
  ): dom.Element =
    model match
      case Model.Menu               => menu(groups, activeSize, catalogError, actions)
      case Model.HowTo              => howTo(actions)
      case Model.Loading(label)     => loading(label)
      case Model.LoadError(message) => loadError(message, actions)
      case play: Model.Play         => playView(play, groups, activeSize, catalogError, actions)

  private def menu(
      groups: Seq[Level.Group],
      activeSize: Option[String],
      catalogError: Option[String],
      actions: Actions
  ): dom.Element =
    val card = el(
      "section",
      "card menu-card",
      gameTitle(),
      el("p", "lede", text("A folding puzzle")),
      el(
        "div",
        "actions",
        button("Start", "btn primary", "start", actions.start),
        button("How to play", "btn secondary", "howto", actions.showHowTo)
      )
    )
    frames(card, levelPicker(groups, activeSize, None, catalogError, actions))

  private def sampleButton(label: String, kind: String): dom.Element =
    el("span", s"btn $kind note-sample", text(label))

  private def gameTitle(heading: Boolean = true): dom.Element =
    val image = dom.document.createElement("img").asInstanceOf[dom.html.Image]
    image.src = "paperfold_title_final.png"
    image.alt = "Paperfold"
    el(if heading then "h1" else "p", "game-title", image)

  private def howTo(actions: Actions): dom.Element =
    stage(
      el(
        "section",
        "card how-card",
        gameTitle(heading = false),
        el("p", "eyebrow", text("how to play")),
        el("h1", "", text("Fold on the creases")),
        el(
          "ol",
          "steps",
          step("1", "Read the target", "Each level lays out a fresh sheet. The stack you must finish with is stamped beside it."),
          step("2", "Choose a crease", "Click the gap between two columns, or the gap between two rows."),
          step("3", "Choose a side", "Click the half that folds over that crease: left or right, upper or lower."),
          step("4", "Match one cell", "Keep folding until a single cell remains. If its stack matches the target, the level is complete.")
        ),
        el(
          "div",
          "notes",
          el(
            "p",
            "note",
            sampleButton("Retry", "secondary"),
            text("lays the sheet flat again.")
          ),
          el(
            "p",
            "note",
            sampleButton("Next", "primary"),
            text("opens the following level after completing a level.")
          )
        ),
        button("Back", "btn secondary", "back", actions.backToMenu)
      )
    )

  private def loading(label: String): dom.Element =
    stage(
      el(
        "section",
        "card menu-card",
        gameTitle(heading = false),
        el("p", "eyebrow", text(s"level $label")),
        el("h1", "", text("Laying out the sheet")),
        el("p", "lede", text("Fetching the level."))
      )
    )

  private def loadError(message: String, actions: Actions): dom.Element =
    stage(
      el(
        "section",
        "card menu-card",
        gameTitle(heading = false),
        el("p", "eyebrow", text("missing sheet")),
        el("h1", "", text("That level did not load")),
        el("p", "lede", text(message)),
        button("Menu", "btn secondary", "back", actions.backToMenu)
      )
    )

  private def playView(
      state: Model.Play,
      groups: Seq[Level.Group],
      activeSize: Option[String],
      catalogError: Option[String],
      actions: Actions
  ): dom.Element =
    val rows = Paper.height(state.paper)
    val cols = Paper.width(state.paper)
    val wrap = el("div", "sheet-wrap")
    prompt(state).foreach(wrap.appendChild)
    legend(state).foreach(wrap.appendChild)
    wrap.appendChild(sheet(state, rows, cols, actions))

    val layout = el("div", "play-layout", wrap, moves(state))
    val title = el("section", "card title-card", gameTitle())
    val card = el("section", "card play-card", header(state, actions), layout)
    banner(state, actions).foreach(card.appendChild)
    frames(title, card, levelPicker(groups, activeSize, Some(state.path), catalogError, actions))

  private def header(state: Model.Play, actions: Actions): dom.Element =
    el(
      "header",
      "topbar",
      el(
        "p",
        "size",
        el("span", "size-label", text("Paper size")),
        text(s"${state.level.height} × ${state.level.width}")
      ),
      el("p", "level-chip", text(s"Level ${state.level.level}")),
      el(
        "div",
        "target-stamp" + (if state.level.height == 8 && state.level.width == 7 then " is-compact" else ""),
        el("span", "", text("Target")),
        el("strong", "", text(state.level.target))
      ),
      button("Menu", "btn tiny", "menu", actions.backToMenu)
    )

  private def prompt(state: Model.Play): Option[dom.Element] =
    val message =
      if state.animating.nonEmpty then "Folding…"
      else
        state.crease match
          case Some(_: Crease.Column) =>
            "Click the left half or the right half to fold it over this crease."
          case Some(_: Crease.Row) =>
            "Click the upper half or the lower half to fold it over this crease."
          case None if state.outcome == Outcome.Playing =>
            "Select a crease between two columns or two rows."
          case None => ""
    if message.isEmpty then None
    else
      val node = el("p", "prompt", text(message))
      node.setAttribute("aria-live", "polite")
      Some(node)

  private def legend(state: Model.Play): Option[dom.Element] =
    if state.animating.nonEmpty then None
    else
      state.crease match
        case Some(_: Crease.Column) =>
          Some(legendRow("left half", "right half"))
        case Some(_: Crease.Row) =>
          Some(legendRow("upper half", "lower half"))
        case None => None

  private def legendRow(green: String, amber: String): dom.Element =
    el(
      "p",
      "legend",
      el("span", "swatch green"),
      text(s" $green"),
      el("span", "swatch amber"),
      text(s" $amber")
    )

  private def sheet(state: Model.Play, rows: Int, cols: Int, actions: Actions): dom.Element =
    val interactive = state.outcome == Outcome.Playing && state.animating.isEmpty
    val node = el("div", sheetClass(state))
    node.style.setProperty("grid-template-columns", tracks(cols, "minmax(3rem, 1fr)"))
    node.style.setProperty("grid-template-rows", tracks(rows, "minmax(3.4rem, auto)"))
    for
      r <- 0 until rows
      c <- 0 until cols
    do node.appendChild(cell(state, r, c, interactive, actions))
    if interactive || state.animating.nonEmpty then addCreases(node, state, rows, cols, interactive, actions)
    node

  private def sheetClass(state: Model.Play): String =
    val armed = if state.crease.nonEmpty && state.animating.isEmpty then " armed" else ""
    val folding = state.animating match
      case Some(Instruction('L', _)) => " is-folding fold-l"
      case Some(Instruction('R', _)) => " is-folding fold-r"
      case Some(Instruction('T', _)) => " is-folding fold-t"
      case Some(Instruction('B', _)) => " is-folding fold-b"
      case _                          => ""
    s"sheet$armed$folding"

  private def cell(state: Model.Play, row: Int, col: Int, interactive: Boolean, actions: Actions): dom.Element =
    val stack = state.paper(row)(col)
    val cls = s"cell ${sideClass(state, row, col)}".trim
    val label = stackLabel(stack)
    val clickable = interactive && state.crease.nonEmpty
    val node =
      if clickable then
        val buttonNode = el("button", cls, label).asInstanceOf[dom.html.Button]
        buttonNode.setAttribute("type", "button")
        buttonNode.setAttribute("data-cell", s"$row-$col")
        buttonNode.setAttribute("aria-label", s"Fold the ${sideName(state, row, col)} half. Cell $stack")
        buttonNode.addEventListener(
          "click",
          (_: dom.Event) => state.crease.foreach(crease => actions.fold(instructionFor(crease, row, col)))
        )
        buttonNode
      else
        val div = el("div", cls, label)
        div.setAttribute("data-cell", s"$row-$col")
        div
    node.style.setProperty("grid-row", (row * 2 + 1).toString)
    node.style.setProperty("grid-column", (col * 2 + 1).toString)
    node

  private def sideClass(state: Model.Play, row: Int, col: Int): String =
    state.crease match
      case Some(Crease.Column(pivot)) => if col < pivot then "left-of-crease" else "right-of-crease"
      case Some(Crease.Row(pivot))    => if row < pivot then "above-crease" else "below-crease"
      case None                        => ""

  private def sideName(state: Model.Play, row: Int, col: Int): String =
    state.crease match
      case Some(Crease.Column(pivot)) => if col < pivot then "left" else "right"
      case Some(Crease.Row(pivot))    => if row < pivot then "upper" else "lower"
      case None                        => ""

  private def instructionFor(crease: Crease, row: Int, col: Int): Instruction =
    crease match
      case Crease.Column(pivot) => Instruction(if col < pivot then 'L' else 'R', pivot)
      case Crease.Row(pivot)    => Instruction(if row < pivot then 'T' else 'B', pivot)

  private def addCreases(
      sheet: dom.Element,
      state: Model.Play,
      rows: Int,
      cols: Int,
      interactive: Boolean,
      actions: Actions
  ): Unit =
    if cols > 1 then
      for pivot <- 1 until cols do
        sheet.appendChild(
          creaseLine(
            state,
            Crease.Column(pivot),
            interactive,
            actions,
            gridColumn = (pivot * 2).toString,
            gridRow = "1 / -1",
            vertical = true,
            name = s"column-$pivot",
            label = s"Column crease $pivot"
          )
        )
    if rows > 1 then
      for pivot <- 1 until rows do
        sheet.appendChild(
          creaseLine(
            state,
            Crease.Row(pivot),
            interactive,
            actions,
            gridColumn = "1 / -1",
            gridRow = (pivot * 2).toString,
            vertical = false,
            name = s"row-$pivot",
            label = s"Row crease $pivot"
          )
        )

  private def creaseLine(
      state: Model.Play,
      crease: Crease,
      interactive: Boolean,
      actions: Actions,
      gridColumn: String,
      gridRow: String,
      vertical: Boolean,
      name: String,
      label: String
  ): dom.Element =
    val selected = state.crease.contains(crease)
    val direction = if vertical then "vertical" else "horizontal"
    val cls = "crease " + direction + (if selected then " is-selected" else "")
    val node = el("button", cls).asInstanceOf[dom.html.Button]
    node.setAttribute("type", "button")
    node.setAttribute("data-crease", name)
    node.setAttribute("aria-label", label)
    node.setAttribute("aria-pressed", if selected then "true" else "false")
    node.style.setProperty("grid-column", gridColumn)
    node.style.setProperty("grid-row", gridRow)
    if interactive then node.addEventListener("click", (_: dom.Event) => actions.selectCrease(crease))
    else node.disabled = true
    node

  private def stackLabel(stack: String): dom.Element =
    val lengthClass =
      if stack.length > 6 then " long"
      else if stack.length > 2 then " medium"
      else ""
    val node = el("span", s"stack$lengthClass")
    if stack.isEmpty then node.appendChild(text("·"))
    else
      node.appendChild(el("span", "top-letter", text(stack.head.toString)))
      if stack.length > 1 then node.appendChild(el("span", "buried", text(stack.tail)))
    node

  private def moves(state: Model.Play): dom.Element =
    val list = el("ol", "move-list")
    if state.moves.isEmpty then list.appendChild(el("li", "empty", text("No folds yet")))
    else
      state.moves.zipWithIndex.foreach { (instruction, index) =>
        list.appendChild(
          el(
            "li",
            "",
            el("span", "move-n", text((index + 1).toString)),
            el("span", "move-code", text(s"${instruction.part} ${instruction.index}")),
            el("span", "move-word", text(moveWord(instruction)))
          )
        )
      }
    el("aside", "moves", el("h2", "", text("Folds")), list)

  private def banner(state: Model.Play, actions: Actions): Option[dom.Element] =
    val got =
      if Paper.height(state.paper) == 1 && Paper.width(state.paper) == 1 then state.paper(0)(0)
      else ""
    state.outcome match
      case Outcome.Failed =>
        Some(
          notice(
            "banner fail",
            el(
              "p",
              "banner-copy",
              text("The sheet folded into "),
              el("strong", "", text(got)),
              text(". The target is "),
              el("strong", "", text(state.level.target)),
              text(".")
            ),
            el("div", "actions", button("Retry", "btn primary", "retry", actions.retry))
          )
        )
      case Outcome.Cleared =>
        val note = state.note match
          case Some(message) => el("p", "last-level", text(message))
          case None          => text("")
        Some(
          notice(
            "banner ok",
            el(
              "p",
              "banner-copy",
              text("Level complete. The stack matches "),
              el("strong", "", text(state.level.target)),
              text(".")
            ),
            note,
            el(
              "div",
              "actions",
              button("Retry", "btn secondary", "retry", actions.retry, state.pendingNext),
              button(
                if state.pendingNext then "Looking…" else "Next",
                "btn primary",
                "next",
                actions.nextLevel,
                state.pendingNext
              )
            )
          )
        )
      case Outcome.Playing =>
        state.note.map(message => notice("banner fail", el("p", "banner-copy", text(message))))

  private def notice(cls: String, kids: dom.Node*): dom.Element =
    val node = el("div", cls, kids*)
    node.setAttribute("role", "status")
    node

  private def step(n: String, title: String, body: String): dom.Element =
    el(
      "li",
      "step",
      el("span", "step-n", text(n)),
      el("div", "step-body", el("h2", "", text(title)), el("p", "", text(body)))
    )

  private def moveWord(instruction: Instruction): String =
    instruction.part match
      case 'L' => "left over right"
      case 'R' => "right over left"
      case 'T' => "top over bottom"
      case 'B' => "bottom over top"
      case _   => ""

  private def levelPicker(
      groups: Seq[Level.Group],
      activeSize: Option[String],
      currentPath: Option[String],
      catalogError: Option[String],
      actions: Actions
  ): dom.Element =
    val picker = el("section", "card level-card", el("h2", "", text("Levels")))
    if groups.isEmpty then
      val message = catalogError.getOrElse("Looking for sheets.")
      picker.appendChild(el("p", "level-empty", text(message)))
    else
      val shown = activeSize.flatMap(size => groups.find(_.size == size)).getOrElse(groups.head)
      val tabs = el("div", "level-tabs")
      tabs.setAttribute("role", "tablist")
      groups.foreach { group =>
        val selected = group.size == shown.size
        val tab = el("button", "level-tab" + (if selected then " is-active" else ""), text(Level.sizeLabel(group.size)))
          .asInstanceOf[dom.html.Button]
        tab.setAttribute("type", "button")
        tab.setAttribute("role", "tab")
        tab.setAttribute("data-size", group.size)
        tab.setAttribute("aria-selected", if selected then "true" else "false")
        tab.addEventListener("click", (_: dom.Event) => actions.selectSize(group.size))
        tabs.appendChild(tab)
      }
      val dots = el("div", "level-dots")
      dots.setAttribute("role", "tabpanel")
      shown.entries.foreach { entry =>
        val selected = currentPath.contains(entry.path)
        val dot = el(
          "button",
          "level-dot" + (if selected then " is-current" else ""),
          text(entry.level.level)
        ).asInstanceOf[dom.html.Button]
        dot.setAttribute("type", "button")
        dot.setAttribute("data-level", entry.level.level)
        dot.setAttribute("data-path", entry.path)
        if selected then dot.setAttribute("aria-current", "true")
        dot.addEventListener("click", (_: dom.Event) => actions.chooseLevel(entry))
        dots.appendChild(dot)
      }
      picker.appendChild(tabs)
      picker.appendChild(dots)
    picker

  private def tracks(count: Int, cell: String): String =
    if count <= 1 then cell
    else List.fill(count)(cell).mkString(" 1.35rem ")

  private def frames(kids: dom.Element*): dom.Element =
    stage(el("div", "frames", kids*))

  private def stage(kids: dom.Node*): dom.Element =
    el("main", "stage", kids*)

  private def button(
      label: String,
      cls: String,
      actionName: String,
      action: () => Unit,
      disabled: Boolean = false
  ): dom.html.Button =
    val node = el("button", cls, text(label)).asInstanceOf[dom.html.Button]
    node.setAttribute("type", "button")
    node.setAttribute("data-action", actionName)
    node.disabled = disabled
    if !disabled then node.addEventListener("click", (_: dom.Event) => action())
    node

  private def el(tag: String, cls: String, kids: dom.Node*): dom.HTMLElement =
    val node = dom.document.createElement(tag).asInstanceOf[dom.HTMLElement]
    if cls.nonEmpty then node.className = cls
    kids.foreach(node.appendChild)
    node

  private def text(value: String): dom.Text =
    dom.document.createTextNode(value)
