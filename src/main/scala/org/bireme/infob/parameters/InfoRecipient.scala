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
                    langCodeSystem: Option[String] = Some(InfoRecipient.ISO_639_1),
                    langDisplayName: Option[String] = None) extends SearchParameter {
  val lCodeSys: String = langCodeSystem.getOrElse("")
  require (lCodeSys.isEmpty || lCodeSys.equals(InfoRecipient.ISO_639_1))

  val lang2: Map[String, String] = ISO639_1_Codes.codes.map { case (k, v) => (k, v.head.toLowerCase) }
  val langN: Map[String, String] = lang2.map { case (k, v) => (v, k) }

  val lcode: Option[String] = langCode match {
    case Some(lcode2) => Some(lcode2.toLowerCase).filter(lang2.contains)
    case None        => langDisplayName.flatMap(la => langN.get(la.toLowerCase))
  }
  val role2: Option[String] = role match {
    case Some("PAT")   => Some("PAT")   // patient
    case Some("PROV")  => Some("PROV")  // healthCareProvider
    case Some("PAYOR") => Some("PAYOR") // payor
    case _             => None
  }

  override def toSrcExpression(env: Seq[SearchParameter]): Option[String] = {
//println(s"InfoRecipient lcode=$lcode")
    //lcode.map(lc => s"(la:${'"'}${Tools.encodeUrl(lc)}${'"'})")
    lcode.map(lc => s"(la:${'\"'}$lc${'\"'})")
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

  override def toString: String =
    s"""InfoRecipient(role: Option[String] = $role,
                      langCodeSystem: Option[String] = $langCodeSystem,
                      langCode: Option[String] = $langCode,
                      langDisplayName: Option[String] = $langDisplayName)"""
}

object InfoRecipient extends Parser {
  val ISO_639_1 = "2.16.840.1.113883.1.11.11526"

  override def parse(conv: MeshConverter,
                     parameters: Map[String, String])
    :(Seq[SearchParameter], Map[String, String]) = {

    val (ir, others) = parameters.partition(_._1.startsWith("informationRecipient"))

    if (ir.isEmpty) {
      (Seq(), others)
    } else {
//println("ir=" + ir)
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
