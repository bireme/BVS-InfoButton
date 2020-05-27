/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

import scala.io.Source
import scala.util.{Try, Success, Failure}

object Tools {
  def getPageContent(url: String,
                     encoding: String = "utf-8"): Option[String] = {
    Try (Source.fromURL(url, encoding)) match {
      case Success(content) => Some(content.getLines.mkString("\n"))
      case Failure(_) => None
    }
  }

  def getNumberOccurrences(text: String,
                           pattern: String): Int = {
    require(text != null)
    require(pattern != null)

    pattern.r.findAllMatchIn(text).size
  }

  def hasStringPattern(text: String,
                       pattern: String): Boolean = {
    require(text != null)
    require(pattern != null)

    val split = text.split("entry", 2)
    if (split.size == 1) false
    else pattern.r.findFirstIn(split(1)).isDefined
  }

  def urlEncode(url: String,
                encoding: String = "utf-8"): String =
    java.net.URLEncoder.encode(url, encoding)
}
