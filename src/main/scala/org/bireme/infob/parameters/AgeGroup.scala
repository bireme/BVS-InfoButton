/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}
import scala.util.{Try, Success, Failure}

class AgeGroup(code: Option[String] = None,
               codeSystem: Option[String] = None,
               displayName: Option[String] = None) extends SearchParameter {
  require(code.isDefined || displayName.isDefined)

  val csystem: String = codeSystem.getOrElse("2.16.840.1.113883.6.177").trim

  val agroup: Option[String] = if (csystem.equals("2.16.840.1.113883.6.177")) {
    code.map(_.trim.toUpperCase) match {
      case Some(cd) =>
        cd match {
          case "D007231" => Some("infant, newborn")
          case "D007223" => Some("infant")
          case "D002675" => Some("child, preschool")
          case "D002648" => Some("child")
          case "D000293" => Some("adolescent")
          case "D055815" => Some("young adult")
          case "D000328" => Some("adult")
          case "D008875" => Some("middle aged")
          case "D000368" => Some("aged")
          case "D000369" => Some("aged, 80 and older")
          case _         => None
        }
      case None =>
        displayName.map(_.toLowerCase) match {
          case Some("infant, newborn; birth to 1 month")  => Some("infant, newborn")
          case Some("infant; 1 to 23 months")             => Some("infant")
          case Some("child, preschool; 2 to 5 years")     => Some("child, preschool")
          case Some("child; 6 to 12 years")               => Some("child")
          case Some("adolescent; 13-18 years")            => Some("adolescent")
          case Some("young adult; 19-24 years")           => Some("young adult")
          case Some("adult; 19-44 years")                 => Some("adult")
          case Some("middle aged; 45-64 years")           => Some("middle aged")
          case Some("aged; 56-79 years")                  => Some("aged")
          case Some("aged, 80 and older; a person 80 years of age and older") => Some("aged, 80 and older")
          case _ => None
        }
    }
  } else {
    None
  }

  override def toSrcExpression(env: Seq[SearchParameter]): Option[String] =
    agroup.map(ag => s"(limit:${'"'}$ag${'"'})")
    //agroup.map(ag => s"(limit:${'"'}${Tools.encodeUrl(ag)}${'"'})")

  override def getCategories: Seq[Category] = {
    Seq(
      Category("ageGroup.v.c", code.getOrElse("")),
      Category("ageGroup.v.cs", codeSystem.getOrElse("")),
      Category("ageGroup.v.dn", displayName.getOrElse(""))
    ).filter(!_.term.isEmpty)
  }

  override def toString: String =
    s"""AgeGroup(code: Option[String] = $code,
                   codeSystem: Option[String] = $codeSystem,
                   displayName: Option[String] = $displayName)"""
}

object AgeGroup extends Parser {
  override def parse(conv: MeshConverter,
                    parameters: Map[String, String]):
                                 (Seq[SearchParameter], Map[String, String]) = {

    val (ag, others) = parameters.partition(_._1.startsWith("ageGroup.v."))

    if (ag.isEmpty) {
      (Seq(), others)
    } else {
      Try (
        new AgeGroup(
          parameters.get("ageGroup.v.c"),
          parameters.get("ageGroup.v.cs"),
          parameters.get("ageGroup.v.dn")
        )
      ) match {
        case Success(s) => (Seq(s), others)
        case Failure(_) => (Seq(), others)
      }
    }
  }
}
