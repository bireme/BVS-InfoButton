/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}
import scala.util.{Try, Success, Failure}

class Age(value: Option[String],
          unit: Option[String]) extends SearchParameter {
  val value2: Int = value.getOrElse("0").toInt
  require(value2 > 0)
  val unit2: String = unit.getOrElse("").toLowerCase
  require(unit2.equals("min") || unit2.equals("h") || unit2.equals("d") ||
          unit2.equals("wk") || unit2.equals("mo") || unit2.equals("a"))

  val agroup: Option[String] = convertToMonth(value.get, unit.get) match {
    case Some(x) if (x >= 0) && (x <= 1)    => Some("infant, newborn")
    case Some(x) if (x > 1) && (x < 24)     => Some("infant")
    case Some(x) if (x >= 24) && (x < 72)   => Some("child, preschool")
    case Some(x) if (x >= 72) && (x < 156)  => Some("child")
    case Some(x) if (x >= 156) && (x < 228) => Some("adolescent")
    case Some(x) if (x >= 228) && (x < 288) => Some("young adult")
    case Some(x) if (x >= 228) && (x < 540) => Some("adult")
    case Some(x) if (x >= 540) && (x < 780) => Some("middle aged")
    case Some(x) if (x >= 780) && (x < 960) => Some("aged")
    case Some(x) if x >= 960                => Some("aged, 80 and older")
    case None                               => None
  }

  private def convertToMonth(value: String, unit: String): Option[Int] = {
    val ival = value.toInt

    unit.toLowerCase match {
      case "min" => Some(ival / (1 * 60 * 24 * 30)) // minute
      case "h"   => Some(ival / (1 * 24 * 30)) // hour
      case "d"   => Some(ival / (1 * 30)) // day
      case "wk"  => Some(ival / (1 * 7)) // week
      case "mo"  => Some(ival) // month
      case "a"   => Some(ival * 12) // year
      case _     => None
    }
  }

  override def toSrcExpression(env: Seq[SearchParameter]): Option[String] =
    agroup.map(ag => s"(limit:${'"'}$ag${'"'})")

  override def getCategories: Seq[Category] =
    Seq(Category("age.v.v", value.get), Category("age.v.u", unit.get))

  override def toString: String =
    s"""Age(value: String = ${value.get},
            unit: String = ${unit.get})"""
}

object Age extends Parser {
  override def parse(conv: MeshConverter,
                     parameters: Map[String, String])
    :(Seq[SearchParameter], Map[String, String]) = {

    val (age, others) = parameters.partition(_._1.startsWith("age.v."))

    if (age.isEmpty) {
      (Seq(), others)
    } else {
      Try (
          new Age(
            age.get("age.v.v"),
            age.get("age.v.u")
          )
      ) match {
        case Success(s) => (Seq(s), others)
        case Failure(_) => (Seq(), others)
      }
    }
  }
}
