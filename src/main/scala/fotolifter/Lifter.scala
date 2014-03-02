package fotolifter

import scala.io.Source
import scala.sys.process._
import java.io.{FileWriter, BufferedWriter, File}
import java.nio.file.{Files, Path, Paths}
import java.util.UUID

case class Line(line: String) {
  val path: Path = Paths.get(line)

  val file: File = path.toFile

  val filename: String = file.getName

  val extension: String = {
    filename.lastIndexOf(".") match {
      case -1 => ""
      case pos => filename.substring(pos + 1).toLowerCase
    }
  }

  val uuid: UUID = UUID.randomUUID()

  def defineTask(destination: Path): Task = {
    if (filename.startsWith(".")) {
      return EmptyTask(line, uuid, path, destination)
    }

    extension match {
      case "jpg" | "jpeg" | "png" => CopyTask(line, uuid, path, destination)
      case "psd" | "nef" | "tif" | "tiff" => ConvertTask(line, uuid, path, destination)
      case _ => EmptyTask(line, uuid, path, destination)
    }
  }
}

trait Task {
  val line: String

  val origin: Path

  val originFilename: String = origin.getFileName.toString

  val destination: Path

  val uuid: UUID

  def execute(): Unit

  val originFilenameBody: String = {
    originFilename.lastIndexOf(".") match {
      case -1 => originFilename
      case pos => originFilename.substring(0, pos)
    }
  }

  lazy val copyName: Path = {
    val originalName = destination.resolve(originFilenameBody + ".jpg")
    if (Files.exists(originalName)) {
      destination.resolve(s"$originFilenameBody $uuid.jpg")
    } else {
      originalName
    }
  }

}

case class EmptyTask(line: String,
                     uuid: UUID,
                     origin: Path,
                     destination: Path)
  extends Task {

  def execute(): Unit = {
  }
}

case class CopyTask(line: String,
                    uuid: UUID,
                    origin: Path,
                    destination: Path)
  extends Task {

  def execute(): Unit = {
    println(s"Copying.. $origin to $copyName")
    Files.copy(origin, copyName)
  }
}

case class ConvertTask(line: String,
                       uuid: UUID,
                       origin: Path,
                       destination: Path)
  extends Task {

  def command(): Seq[String] = Seq(
    "sips",
    "-s",
    "format",
    "jpeg",
    origin.toAbsolutePath.toString,
    "--out",
    copyName.toAbsolutePath.toString
  )

  override def execute(): Unit = {
    val cmd = command()
    println(cmd)
    cmd
      .!!
  }
}

case class Lifter(journal: Path,
                  source: Path,
                  destination: Path) {

  val journalWriter = new BufferedWriter(new FileWriter(journal.toFile, true))

  def lift(): Unit = {
    Source.fromFile(source.toFile)
      .getLines()
      .map(Line)
      .map(l => l.defineTask(destination))
      .toList
      .par
      .foreach {
      l =>
        try {
          l.execute()
          journalWriter.write(l.line + "\n")
        } catch {
          case e: Throwable =>
            e.printStackTrace()
        }
    }
  }
}

object Lifter {
  def main(args: Array[String]) {
    if (args.length < 3) {
      println(s"Lifter [journal file] [source file] [destination]")
    } else {
      Lifter(Paths.get(args(0)), Paths.get(args(1)), Paths.get(args(2))).lift()
    }
  }
}
