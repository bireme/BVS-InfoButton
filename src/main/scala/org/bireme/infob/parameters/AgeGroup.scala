/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}

class AgeGroup(code: Option[String] = None,
               codeSystem: Option[String] = None,
               displayName: Option[String] = None) extends SearchParameter {
  require (!code.isEmpty || !displayName.isEmpty)

  val csystem = codeSystem.getOrElse("MESH").toUpperCase
  require (csystem.equals("MESH"))

  val agroup = code.map(_.toUpperCase) match {
    case Some(cd) => cd match {
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
      case _ => None
    }
    case None => displayName.map(_.toLowerCase) match {
        case Some("infant, newborn") => Some("infant, newborn")
        case Some("infant") => Some("infant")
        case Some("child, preschool") => Some("child, preschool")
        case Some("child") => Some("child")
        case Some("adolescent") => Some("adolescent")
        case Some("young adult") => Some("young adult")
        case Some("adult") => Some("adult")
        case Some("aged") => Some("aged")
        case Some("middle aged") => Some("middle aged")
        case Some("aged, 80 and older") => Some("aged, 80 and older")
        case Some("birth to 1 month") => Some("infant, newborn")
        case Some("1 to 23 months") => Some("infant")
        case Some("2 to 5 years") => Some("child, preschool")
        case Some("6 to 12 years") => Some("child")
        case Some("13-18 years") => Some("adolescent")
        case Some("19-24 years") => Some("young adult")
        case Some("19-44 years") => Some("adult")
        case Some("56-79 years") => Some("aged")
        case Some("45-64 years") => Some("middle aged")
        case Some("a person 80 years of age and older") => Some("aged, 80 and older")
        case _ => None
    }
  }
println(s"agroup=$agroup")
  override def toSrcExpression(conv: MeshConverter): Option[String] =
    agroup.map(ag => s"(limit:(%22$ag%22))")

  override def getCategories: Seq[Category] = {
    Seq(
      Category("ageGroup.v.c",  code.getOrElse("")),
      Category("ageGroup.v.cs", codeSystem.getOrElse("")),
      Category("ageGroup.v.dn", displayName.getOrElse(""))
    ).filter(!_.term.isEmpty)
  }

  override def toString =
    s"""AgeGroup(code: Option[String] = $code,
                   codeSystem: Option[String] = $codeSystem,
                   displayName: Option[String] = $displayName)"""
}

object AgeGroup extends Parser {
  override def parse(parameters: Map[String,String]): Option[AgeGroup] = {
    parameters.find(_._1.startsWith("ageGroup.v.")) match {
      case Some(_) => Some(new AgeGroup(
        parameters.get("ageGroup.v.c"),
        parameters.get("ageGroup.v.cs"),
        parameters.get("ageGroup.v.dn")
      ))
      case None => None
    }
  }
}
