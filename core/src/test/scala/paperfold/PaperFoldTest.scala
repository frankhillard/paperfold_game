package paperfold

class PaperFoldTest extends munit.FunSuite:

  test("2x2 paper is filled row-major from A"):
    val paper = Paper.create(2, 2)
    assertEquals(toLists(paper), List(List("A", "B"), List("C", "D")))

  test("create names cells from the given alphabet"):
    assertEquals(toLists(Paper.create(1, 3, "XYZ")), List(List("X", "Y", "Z")))

  test("level 1 target BACD is L 1 then T 1"):
    val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val paper = Paper.create(2, 2, alphabet)
    val folded = Folder.applyAll(paper, List(Instruction('L', 1), Instruction('T', 1)))
    assertEquals(toLists(folded), List(List("BACD")))

  test("3x3 paper with L 2, B 1, R 1, T 1 should be HIGADFEBC"):
    val paper = Paper.create(3, 3)
    assertEquals(
      toLists(paper),
      List(
        List("A", "B", "C"),
        List("D", "E", "F"),
        List("G", "H", "I")
      )
    )
    val folded = Folder.applyAll(
      paper,
      List(Instruction('L', 2), Instruction('B', 1), Instruction('R', 1), Instruction('T', 1))
    )
    assertEquals(toLists(folded), List(List("HIGADFEBC")))

  test("4x3 paper continues naming on the next row"):
    val paper = Paper.create(3, 4)
    assertEquals(
      toLists(paper),
      List(
        List("A", "B", "C", "D"),
        List("E", "F", "G", "H"),
        List("I", "J", "K", "L")
      )
    )

  test("7x8 paper uses lowercase and symbols from the hardcoded alphabet"):
    val paper = Paper.create(8, 7)
    assertEquals(
      toLists(paper),
      List(
        List("A", "B", "C", "D", "E", "F", "G"),
        List("H", "I", "J", "K", "L", "M", "N"),
        List("O", "P", "Q", "R", "S", "T", "U"),
        List("V", "W", "X", "Y", "Z", "a", "b"),
        List("c", "d", "e", "f", "g", "h", "i"),
        List("j", "k", "l", "m", "n", "o", "p"),
        List("q", "r", "s", "t", "u", "v", "w"),
        List("x", "y", "z", "!", "#", "$", "%")
      )
    )

  test("T 1 then L 1 on 2x2 yields CABD"):
    val paper = Paper.create(2, 2)
    val folded = Folder.applyAll(
      paper,
      List(Instruction('T', 1), Instruction('L', 1))
    )
    assertEquals(toLists(folded), List(List("CABD")))

  test("R1 on 4x3 yields D C BA; H G FE; L K JI"):
    val paper = Paper.create(3, 4)
    val folded = Folder.applyAll(paper, List(Instruction('R', 1)))
    assertEquals(
      toLists(folded),
      List(
        List("D", "C", "BA"),
        List("H", "G", "FE"),
        List("L", "K", "JI")
      )
    )

  test("R2 on 4x3 yields DA CB; HE GF; LI KJ"):
    val paper = Paper.create(3, 4)
    val folded = Folder.applyAll(paper, List(Instruction('R', 2)))
    assertEquals(
      toLists(folded),
      List(
        List("DA", "CB"),
        List("HE", "GF"),
        List("LI", "KJ")
      )
    )

  test("R2,B2,T1,L1 on 4x3 yields EHLIDABCJKGF"):
    val paper = Paper.create(3, 4)
    val folded = Folder.applyAll(
      paper,
      List(Instruction('R', 2), Instruction('B', 2), Instruction('T', 1), Instruction('L', 1))
    )
    assertEquals(toLists(folded), List(List("EHLIDABCJKGF")))

  test("R2,T1,R1,B1 on 3x3 yields GIHEFCBAD"):
    val paper = Paper.create(3, 3)
    val folded = Folder.applyAll(
      paper,
      List(Instruction('R', 2), Instruction('T', 1), Instruction('R', 1), Instruction('B', 1))
    )
    assertEquals(toLists(folded), List(List("GIHEFCBAD")))

  test("L 2 on 4 columns folds 0 onto 3 and 1 onto 2"):
    val paper = Paper.create(3, 4)
    val folded = Folder.fold(paper, Instruction('L', 2))
    assertEquals(
      toLists(folded),
      List(
        List("BC", "AD"),
        List("FG", "EH"),
        List("JK", "IL")
      )
    )

  test("R 3 on 4 columns folds column 3 over column 2"):
    val paper = Paper.create(3, 4)
    val folded = Folder.fold(paper, Instruction('R', 3))
    assertEquals(
      toLists(folded),
      List(
        List("A", "B", "DC"),
        List("E", "F", "HG"),
        List("I", "J", "LK")
      )
    )

  test("folding flips an already stacked cell"):
    val afterTop = Folder.fold(Paper.create(2, 2), Instruction('T', 1))
    assertEquals(toLists(afterTop), List(List("AC", "BD")))
    val afterLeft = Folder.fold(afterTop, Instruction('L', 1))
    assertEquals(toLists(afterLeft), List(List("CABD")))

  test("FOLD L4,T3,B4,B2,L2,L1,T1 on 7x8 yields the full stack"):
    val paper = Paper.create(8, 7)
    val folded = Folder.applyAll(
      paper,
      List(
        Instruction('L', 4),
        Instruction('T', 3),
        Instruction('B', 4),
        Instruction('B', 2),
        Instruction('L', 2),
        Instruction('L', 1),
        Instruction('T', 1)
      )
    )
    assertEquals(
      toLists(folded),
      List(List("VOxqut!#SRYZaXQT$zsvwry%UPWbidINGBkpolCFMJehgfKLEDmnjAHc"))
    )

  private def toLists(paper: Array[Array[String]]): List[List[String]] =
    paper.map(_.toList).toList
