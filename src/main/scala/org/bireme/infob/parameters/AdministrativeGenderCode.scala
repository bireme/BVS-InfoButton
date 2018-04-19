/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}
import scala.util.{Try, Success, Failure}

//The gender of a person used for administrative purposes.
class AdministrativeGenderCode(code: Option[String] = None,
                               displayName: Option[String] = None)
    extends SearchParameter {
  require(!code.isEmpty || !displayName.isEmpty)

  val agcode = code.map(_.toLowerCase) match {
    case Some("f")  => Some("female")
    case Some("m")  => Some("male")
    case Some("un") => None // Sugestão do RTM
    case _ =>
      displayName.map(_.toLowerCase) match {
        case Some("female")           => Some("female")
        case Some("male")             => Some("male")
        case Some("undifferentiated") => None
        case _                        => None
      }
  }

  override def toSrcExpression(conv: MeshConverter,
                               env: Seq[SearchParameter]): Option[String] =
    agcode.map(agc => s"(limit:${'"'}$agc${'"'})")

  override def getCategories: Seq[Category] = {
    Seq(
      Category("patientPerson.administrativeGenderCode.c", code.getOrElse("")),
      Category("patientPerson.administrativeGenderCode.dn",
               displayName.getOrElse(""))
    ).filter(!_.term.isEmpty)
  }

  override def toString: String =
    s"""AdministrativeGenderCode(code: Option[String] = $code,
                                 displayName: Option[String] = $displayName)"""
}

object AdministrativeGenderCode extends Parser {
  override def parse(parameters: Map[String, String])
    :(Seq[SearchParameter], Map[String, String]) = {

    val (agc, others) = parameters.partition(_._1.startsWith(
      "patientPerson.administrativeGenderCode."))

    if (agc.isEmpty) (Seq(), others)
    else {
      Try (
          new AdministrativeGenderCode(
            agc.get("patientPerson.administrativeGenderCode.c"),
            agc.get("patientPerson.administrativeGenderCode.dn")
          )
      ) match {
        case Success(s) => (Seq(s), others)
        case Failure(_) => (Seq(), others)
      }
    }
  }
}
