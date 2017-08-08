/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

import java.util.Date
import java.text.SimpleDateFormat
import scala.util.{Try, Success, Failure}

/**
  * Object to convert dates into an output date string with the
  * yyyy-MM-dd'T'HH:mm:ss'Z format
  *
  * @author Heitor Barbieri
  */
object FormatDate {
  val dateFmtIn = new SimpleDateFormat("yyyyMMdd")
  val dateFmtOut = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")

  /**
    * Try to convert the date into the yyyy-MM-dd'T'HH:mm:ss'Z output format
    *
    * @param input date string with the following format: yyyyMMdd
    * @return possibly the formated output date string
    */
  def format1(date: String): Option[String] = {
    Try (dateFmtOut.format(dateFmtIn.parse(date))) match {
      case Success(dt) => Some(dt)
      case Failure(_) => None
    }
  }

  /**
    * Convert the date into the yyyy-MM-dd'T'HH:mm:ss'Z output format
    *
    * @param input date object
    * @return the formated output date string
    */
  def format2(date: Date) = dateFmtOut.format(date)
}
