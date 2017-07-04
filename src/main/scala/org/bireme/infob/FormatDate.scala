/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

import java.util.Date
import java.text.SimpleDateFormat
import scala.util.{Try, Success, Failure}

object FormatDate {
  val dateFmtIn = new SimpleDateFormat("yyyyMMdd")
  val dateFmtOut = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")

  def format1(date: String): Option[String] = {
    Try (dateFmtOut.format(dateFmtIn.parse(date))) match {
      case Success(dt) => Some(dt)
      case Failure(_) => None
    }
  }

  def format2(date: Date) = dateFmtOut.format(date)
}
