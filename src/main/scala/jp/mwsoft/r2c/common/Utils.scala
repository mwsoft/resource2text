package jp.mwsoft.r2c.common

object Utils {

  def managed[T, U <: { def close() }](resource: U)(f: U => T): T = {
    try f(resource)
    finally resource.close()
  }

  def cleanWikiText(str: String): String = removeWikiSymbols(removeDisusedText(removeTags(str)))

  def removeTags(str: String): String = {
    str
      .replaceAll("""<[^>]*>""", "")
      .replaceAll("&amp;", "&")
      .replaceAll("&lt;", "<")
      .replaceAll("&gt;", ">")
      .replaceAll("&nbsp;", " ")
      .replaceAll("&minus;", "-")
      .replaceAll("""style="[^"]+"""", "")
      .replaceAll("""class="[^"]+"""", "")
      .replaceAll("""rowspan="[0-9]+"""", "")
      .replaceAll("""colspan="[0-9]+"""", "")
      .replaceAll("""bgcolor="[a-zA-Z\#0-9]+"""", "")
      .replaceAll("""align="[a-zA-Z]+"""", "")
      .trim
  }

  def removeWikiSymbols(str: String): String = {
    str
      .replaceAll("""(\[|\])+""", "")
      .replaceAll("""(\{|\})+""", "")
      .replaceAll("""==+""", "")
      .replaceAll("""'''""", "")
      .replaceAll("""\|""", " ")
      .replaceAll("""^\s?[\:\!\;\|\'\+\=\*\#\-]+""", "")
      .replaceAll("""[fF]lagicon""", "")
  }

  def removeDisusedText(str: String): String = {
    if (str.startsWith("DEFAULTSORT")
      || str.startsWith("Lang")
      || str.startsWith("Link")
      || str.startsWith("-")
      || str.contains("ファイル:"))
      ""
    else
      str
        .replaceAll("""Category:""", "")
        .replaceAll("""^REDIRECT""", "")
        .replaceAll("""https?\:\/\/[0-9a-zA-Z\.=_\+\-\/\:\%\#\$&\?\(\)~]+""", "")
        .replaceAll("""lang[\s-|]+[a-z]+ """, "")
  }

}