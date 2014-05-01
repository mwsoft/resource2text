package jp.mwsoft.r2c.wikipedia

import java.io.BufferedWriter
import java.io.File
import java.io.FileNotFoundException
import java.io.FileWriter
import org.apache.commons.cli.Options
import org.apache.commons.cli.PosixParser
import jp.mwsoft.r2c.common.Utils
import jp.mwsoft.r2c.common.Wakati

object Main extends App {

  def usage() = println("Usage: ./activator 'run [--wakati] xml_file_path'")

  val options = new Options()
  options.addOption("wakati", false, "execute kuromoji wakati")
  val cmd = new PosixParser().parse(options, args)

  if (cmd.getArgs.length == 0) {
    usage()
    sys.exit()
  }

  val wakati = cmd.hasOption("wakati")

  val infile = cmd.getArgs.head

  if (!new File(infile).exists)
    throw new FileNotFoundException(s"file: {${infile}} not found")

  val parser = new PageArticleParser(infile)
  Utils.managed(new BufferedWriter(new FileWriter(s"${infile}.txt"))) { writer =>
    for (
      page <- parser;
      if page.title != null;
      if !page.title.trim.isEmpty;
      if !page.title.startsWith("Wikipedia:");
      if !page.title.startsWith("Help:");
      if !page.title.startsWith("ファイル:");
      if !page.title.contains("曖昧さ回避");
      line <- page.text.split("\n")
    ) {
      val str = Utils.cleanWikiText(line).trim
      if (str.length > 0) {
        if (wakati) writer.write(Wakati.tokenize(str) + "\n")
        else writer.write(str + "\n")
      }
    }
  }
}