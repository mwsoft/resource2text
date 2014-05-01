package jp.mwsoft.r2c.aozora

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.io.IOException
import java.io.InputStream
import java.util.zip.ZipFile
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.io.Codec.string2codec
import scala.io.Source
import org.apache.commons.cli.Options
import org.apache.commons.cli.PosixParser
import org.apache.commons.io.IOUtils

object Aozora2Text extends App {

  def usage() = println("Usage: ./activator 'run [--wakati] cards_dir out_dir'")

  val options = new Options()
  options.addOption("wakati", false, "execute kuromoji wakati")
  val cmd = new PosixParser().parse(options, args)

  if (cmd.getArgs.length < 2) {
    usage()
    sys.exit()
  }

  // get args
  val cardsDir = new File(cmd.getArgs()(0))
  val outDir = new File(cmd.getArgs()(1))
  val wakati = cmd.hasOption("wakati")

  if (!cardsDir.exists)
    throw new IOException(s"cardd_dir:${cardsDir} not found")

  // create output directory
  if (outDir.exists && !outDir.isDirectory())
    throw new IOException(s"out_dir:${outDir} already exists")
  if (!outDir.exists && !outDir.mkdirs())
    throw new IOException(s"out_dir:${outDir} cannot create")

  // execute create file
  execute(cardsDir, outDir, wakati)

  def execute(cardsDir: File, outDir: File, wakati: Boolean = false) {
    // read files
    var counter = 0
    for (
      card <- cardsDir.listFiles if card.isDirectory && card.getName.matches("[0-9]+");
      files <- card.listFiles if files.isDirectory && files.getName == "files";
      file <- files.listFiles if file.getName.endsWith(".zip");
      zipFile <- try Some(new ZipFile(file.getPath)) catch { case e: Throwable => None }
    ) {
      counter += 1
      val entries = zipFile.entries
      while (entries.hasMoreElements) {
        val entry = entries.nextElement()
        if (entry.getName.endsWith(".txt")) {
          val content = inputStream2text(zipFile.getInputStream(entry))
          // write 
          val writer = new BufferedWriter(new FileWriter(new File(outDir, entry.getName.split("/").last)))
          try writer.write(content) finally writer.close()
        }
      }
    }
    println(counter)
  }

  private def inputStream2text(is: InputStream) = try {
    val lines = IOUtils.lineIterator(is, "Shift_JIS").toList
    val size = lines.size

    object State {
      val title = 0
      val header = 1
      val content = 2
      val footer = 3
    }

    def cleanLine(line: String) = {
      line.replaceAll("《[^》]+》", "").replaceAll("""［[^］]+\］""", "").replaceAll("""｜""", "")
    }

    var state = State.title
    val builder = new StringBuilder()
    for ((line, idx) <- lines.zipWithIndex) yield {
      if (state == State.title) {
        if (line.startsWith("------------------------------------"))
          state = State.header
      } else if (state == State.header) {
        if (line.startsWith("------------------------------------"))
          state = State.content
      } else if (state == State.content) {
        if (idx + 20 > size && line.startsWith("底本：")) state = State.footer
        else if (line.trim.size > 0) builder append cleanLine(line).+("\n")
      }
    }
    builder.toString
  } finally is.close()
}