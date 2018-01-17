/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category,MeshConverter}

//The gender of a person used for administrative purposes.
class AdministrativeGenderCode(code: Option[String] = None,
                               displayName: Option[String] = None)
                                                       extends SearchParameter {
  require (!code.isEmpty || !displayName.isEmpty)

  val agcode = code.map(_.toLowerCase) match {
    case Some("f") => Some("female")
    case Some("m") => Some("male")
    case Some("un") => None  // Sugestão do RTM
    case _ => displayName.map(_.toLowerCase) match {
      case Some("female") => Some("female")
      case Some("male") => Some("male")
      case Some("undifferentiated") => None
      case _ => None
    }
  }

  override def toSrcExpression(conv: MeshConverter,
                               env: Seq[SearchParameter]): Option[String] =
    agcode.map(agc => s"(limit:(%22$agc%22))")

  override def getCategories: Seq[Category] = {
    Seq(
      Category("patientPerson.administrativeGenderCode.c",  code.getOrElse("")),
      Category("patientPerson.administrativeGenderCode.dn", displayName.getOrElse(""))
    ).filter(!_.term.isEmpty)
  }

  override def toString: String =
    s"""AdministrativeGenderCode(code: Option[String] = $code,
                                 displayName: Option[String] = $displayName)"""
}

object AdministrativeGenderCode extends Parser {
  override def parse(parameters: Map[String,String]):
                                            Option[AdministrativeGenderCode] = {
    parameters.find(_._1.startsWith("patientPerson.administrativeGenderCode."))
      match {
        case Some(_) => Some(new AdministrativeGenderCode(
          parameters.get("patientPerson.administrativeGenderCode.c"),
          parameters.get("patientPerson.administrativeGenderCode.dn")
        ))
        case None => None
      }
  }
}
