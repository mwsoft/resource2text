package jp.mwsoft.r2c.common

import java.lang.Character.UnicodeBlock
import org.atilika.kuromoji.Tokenizer
import scala.collection.JavaConversions._
import org.atilika.kuromoji.Token

object Wakati {

  lazy val tokenizer = Tokenizer.builder.build

  case class TokenInfo(
    val surface: String, val katakana: Boolean,
    val lastName: Boolean, val firstName: Boolean,
    val nakaguro: Boolean) {

    def this(token: Token) {
      this(
        token.getSurfaceForm,
        token.getSurfaceForm.forall(c => UnicodeBlock.of(c) == UnicodeBlock.KATAKANA && c != '・'),
        token.getAllFeatures.contains("人名,姓"),
        token.getAllFeatures.contains("人名,名"),
        token.getSurfaceForm == "・")
    }
  }

  def tokenize(str: String): String = {
    val tokens = tokenizer.tokenize(str)
    val builder = new StringBuilder()

    var prevTokenInfo: Option[TokenInfo] = None
    for (token <- tokens) {
      val info = new TokenInfo(token)
      for (prev <- prevTokenInfo) {
        val splitStr =
          if (info.katakana && prev.katakana) ""
          else if (info.firstName && prev.lastName) ""
          else if (info.nakaguro) ""
          else if (prev.nakaguro) ""
          else " "
        builder.append(splitStr)
      }
      builder.append(Option(token.getBaseForm).getOrElse(token.getSurfaceForm))
      prevTokenInfo = Some(info)
    }
    builder.toString
  }

}