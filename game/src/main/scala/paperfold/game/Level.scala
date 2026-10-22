package paperfold.game

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scala.scalajs.js.Thenable.Implicits.given

import org.scalajs.dom

import paperfold.Paper

object Level:
  final case class Entry(number: Int, level: Def)
  final case class Def(level: String, height: Int, width: Int, alphabet: String, target: String)

  def load(n: Int): Future[Option[Def]] =
    dom.fetch(s"levels/level$n.json").toFuture.flatMap { response =>
      if response.status == 404 then Future.successful(None)
      else if response.ok then response.text().toFuture.map(text => Some(parse(text)))
      else Future.failed(new IllegalStateException(s"Could not load level $n (HTTP ${response.status})"))
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

  private def stringField(json: String, name: String): Option[String] =
    s""""$name"\\s*:\\s*"([^"]*)"""".r.findFirstMatchIn(json).map(_.group(1))

  private def intField(json: String, name: String): Option[Int] =
    s""""$name"\\s*:\\s*(\\d+)""".r.findFirstMatchIn(json).map(_.group(1).toInt)
