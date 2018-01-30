/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}

class Age(value: String,
          unit: String) extends SearchParameter {
  require (value != null)
  require (value.toInt > 0)
  require (unit != null)

  val agroup = (convertToMonth(value, unit) match {
    case Some(x) if ((x >= 0) && (x <= 1)) => Some("infant, newborn")
    case Some(x) if ((x > 1) && (x < 24)) => Some("infant")
    case Some(x) if ((x >= 24) && (x < 72)) => Some("child, preschool")
    case Some(x) if ((x >= 72) && (x < 156)) => Some("child")
    case Some(x) if ((x >= 156) && (x < 228)) => Some("adolescent")
    case Some(x) if ((x >= 228) && (x < 288)) => Some("young adult")
    case Some(x) if ((x >= 228) && (x < 540)) => Some("adult")
    case Some(x) if ((x >= 540) && (x < 780)) => Some("middle aged")
    case Some(x) if ((x >= 780) && (x < 960)) => Some("aged")
    case Some(x) if (x >= 960) => Some("aged, 80 and older")
    case None => None
  }).map(_.replace(" ", "%20"))

  private def convertToMonth(value: String,
                             unit: String): Option[Int] = {
    val ival = value.toInt

    unit.toLowerCase match {
      case "min" => Some(ival / (1 * 60 * 24 * 30))  // minute
      case "h"   => Some(ival / (1 * 24 * 30))       // hour
      case "d"   => Some(ival / (1 * 30))            // day
      case "wk"  => Some(ival / (1 * 7))             // week
      case "mo"  => Some(ival)                       // month
      case "a"   => Some(ival * 12)                  // year
      case _ => None
    }
  }

  override def toSrcExpression(conv: MeshConverter,
                               env: Seq[SearchParameter]): Option[String] =
    agroup.map(ag => s"(limit:(%22$ag%22))")

  override def getCategories: Seq[Category] =
    Seq(Category("age.v.v", value), Category("age.v.u", unit))

  override def toString =
    s"""Age(value: String = $value,
            unit: String = $unit)"""
}

object Age extends Parser {
  override def parse(parameters: Map[String,String]): Option[Age] = {
    parameters.get("age.v.v").flatMap(
      v => parameters.get("age.v.u").map(u => new Age(v,u)))
  }
}
