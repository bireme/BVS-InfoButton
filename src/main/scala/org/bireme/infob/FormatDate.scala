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
  val dateFmtOut2 = new SimpleDateFormat("yyyy-MM-dd")

  /**
    * Try to convert the date into the yyyy-MM-dd'T'HH:mm:ss'Z output format
    *
    * @param input date string with the following format: yyyyMMdd
    * @return possibly the formated output date string
    */
  def format1(date: String): Option[String] = {
    Try(dateFmtOut.format(dateFmtIn.parse(date))) match {
      case Success(dt) => Some(dt)
      case Failure(_) =>
        Try(dateFmtOut.format(dateFmtOut2.parse(date))) match {
          case Success(dt) => Some(dt)
          case Failure(_)  => None
        }
    }
  }

  /**
    * Convert the date into the yyyy-MM-dd'T'HH:mm:ss'Z output format
    *
    * @param input date object
    * @return the formated output date string
    */
  def format2(date: Date) = dateFmtOut.format(date)

  /**
    * Convert the date into the yyyy-MM-dd output format
    *
    * @param date date string
    * @return the formated output date string
    */
  def format3(date: String): Option[String] = {
    Try(dateFmtOut2.format(dateFmtIn.parse(date))) match {
      case Success(dt) => Some(dt)
      case Failure(_) =>
        Try(dateFmtOut2.format(dateFmtOut2.parse(date))) match {
          case Success(dt) => Some(dt)
          case Failure(_)  => None
        }
    }
  }
}
