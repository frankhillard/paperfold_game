package paperfold.game

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scala.scalajs.js.Thenable.Implicits.given

import org.scalajs.dom

import paperfold.Paper

object Level:
  final case class Entry(size: String, file: String, path: String, level: Def)
  final case class Group(size: String, entries: Vector[Entry])
  final case class Def(level: String, height: Int, width: Int, alphabet: String, target: String)

  def catalog(): Future[Vector[Group]] =
    listing("levels/").flatMap { names =>
      val sizes = names.filter(isFolder).map(folderName).sortBy(sizeKey)
      Future.traverse(sizes)(loadGroup).map(_.toVector)
    }

  def load(path: String): Future[Option[Def]] =
    dom.fetch(path).toFuture.flatMap { response =>
      if response.status == 404 then Future.successful(None)
      else if response.ok then response.text().toFuture.map(text => Some(parse(text)))
      else Future.failed(new IllegalStateException(s"Could not load $path (HTTP ${response.status})"))
    }

  def parse(json: String): Def =
    Def(
      level = stringField(json, "level").getOrElse(
        intField(json, "level").map(_.toString).getOrElse(
          throw new IllegalArgumentException("level is missing")
        )
      ),
      height = intField(json, "height").getOrElse(throw new IllegalArgumentException("height is missing")),
      width = intField(json, "width").getOrElse(throw new IllegalArgumentException("width is missing")),
      alphabet = stringField(json, "alphabet").getOrElse(Paper.alphabet),
      target = stringField(json, "target").getOrElse(throw new IllegalArgumentException("target is missing"))
    )

  def sizeLabel(size: String): String =
    val sized = """(\d+)x(\d+)""".r
    size match
      case sized(rows, cols) => s"$rows × $cols"
      case other              => other

  private def loadGroup(size: String): Future[Group] =
    listing(s"levels/$size/").flatMap { names =>
      val files = names.filter(_.endsWith(".json"))
      Future.traverse(files) { file =>
        val path = s"levels/$size/$file"
        load(path).flatMap {
          case Some(level) => Future.successful(Entry(size, file, path, level))
          case None        => Future.failed(new IllegalStateException(s"Missing $path"))
        }
      }.map(entries => Group(size, entries.sortBy(entry => levelOrder(entry.level.level)).toVector))
    }

  private def listing(url: String): Future[List[String]] =
    dom.fetch(url).toFuture.flatMap { response =>
      if response.ok then response.text().toFuture.map(parseLinks)
      else Future.failed(new IllegalStateException(s"Could not read $url (HTTP ${response.status})"))
    }

  private def parseLinks(html: String): List[String] =
    """href="([^"]+)"""".r
      .findAllMatchIn(html)
      .map(_.group(1).takeWhile(_ != '?').stripPrefix("./"))
      .filter(href => href.nonEmpty && !href.startsWith("/") && !href.startsWith("../") && href != "../")
      .toList
      .distinct

  private def isFolder(href: String): Boolean =
    href.endsWith("/") && !href.contains("..")

  private def folderName(href: String): String =
    href.stripSuffix("/")

  private def sizeKey(size: String): (Int, Int, String) =
    val sized = """(\d+)x(\d+)""".r
    size match
      case sized(a, b) => (a.toInt, b.toInt, size)
      case other       => (Int.MaxValue, Int.MaxValue, other)

  private def levelOrder(label: String): (Int, String) =
    label.toIntOption match
      case Some(n) => (n, label)
      case None    => (Int.MaxValue, label)

  private def stringField(json: String, name: String): Option[String] =
    s""""$name"\\s*:\\s*"([^"]*)"""".r.findFirstMatchIn(json).map(_.group(1))

  private def intField(json: String, name: String): Option[Int] =
    s""""$name"\\s*:\\s*(\\d+)""".r.findFirstMatchIn(json).map(_.group(1).toInt)
