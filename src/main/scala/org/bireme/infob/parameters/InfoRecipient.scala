/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}
import scala.util.{Try, Success, Failure}

class InfoRecipient(role: Option[String],
                    personCode: Option[String] = None,
                    personCodeSystem: Option[String] = Some("2.16.840.1.113883.6.101"),
                    personDisplayValue: Option[String] = None,
                    langCode: Option[String] = None,
                    langCodeSystem: Option[String] = None,
                    langDisplayName: Option[String] = None) extends SearchParameter {
  val lang2 = ISO639_1_Codes.codes.map { case (k, v) => (k, v.head.toLowerCase) }
  val langN = lang2.map { case (k, v) => (v, k) }

  val lcode: Option[String] = langCodeSystem match {
    case Some("ISO 639-1") =>
      langCode
        .map(_.toLowerCase)
        .flatMap(la => if (lang2.contains(la)) Some(la) else None)
    case Some("") =>
      langCode
        .map(_.toLowerCase)
        .flatMap(la => if (lang2.contains(la)) Some(la) else None)
    case None =>
      langCode
        .map(_.toLowerCase)
        .flatMap(la => if (lang2.contains(la)) Some(la) else None)
    case _ => langDisplayName.flatMap(la => langN.get(la.toLowerCase))
  }

  val role2 = role match {
    case Some("PAT")   => Some("PAT")   // patient
    case Some("PROV")  => Some("PROV")  // healthCareProvider
    case Some("PAYOR") => Some("PAYOR") // payor
    case _             => None
  }

  override def toSrcExpression(conv: MeshConverter,
                               env: Seq[SearchParameter]): Option[String] = {
println(s"***lcode=$lcode")
    lcode.map(lc => s"(la:(%22$lc%22))")
  }

  override def getCategories: Seq[Category] = {
    Seq(
      Category("informationRecipient", role2.getOrElse("")),
      Category("informationRecipient.languageCode.c", langCode.getOrElse("")),
      Category("informationRecipient.languageCode.cs",
               langCodeSystem.getOrElse("")),
      Category("informationRecipient.languageCode.dn",
               langDisplayName.getOrElse(""))
    ).filter(!_.term.isEmpty)
  }

  override def toString =
    s"""InfoRecipient(role: Option[String] = $role,
                      langCodeSystem: Option[String] = $langCodeSystem,
                      langCode: Option[String] = $langCode,
                      langDisplayName: Option[String] = $langDisplayName)"""
}

object InfoRecipient extends Parser {
  override def parse(parameters: Map[String, String])
    :(Seq[SearchParameter], Map[String, String]) = {

    val (ir, others) = parameters.partition(_._1.startsWith("informationRecipient"))

    if (ir.isEmpty) (Seq(), others)
    else {
println("ir=" + ir)
      Try (
        new InfoRecipient(
          role = ir.get("informationRecipient"),
          langCode = ir.get("informationRecipient.languageCode.c"),
          langCodeSystem = ir.get("informationRecipient.languageCode.cs"),
          langDisplayName = ir.get("informationRecipient.languageCode.dn")
        )
      ) match {
        case Success(s) => (Seq(s), others)
        case Failure(_) => (Seq(), others)
      }
    }
  }
}
