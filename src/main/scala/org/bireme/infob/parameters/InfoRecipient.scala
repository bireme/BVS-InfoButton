/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}

class InfoRecipient(role: Option[String],
                    langCode: Option[String] = None,
                    langCodeSystem: Option[String] = None,
                    langDisplayName: Option[String] = None)
                                                       extends SearchParameter {
  val lang2 = Map("en" -> "english", "es" -> "spanish", "pt" -> "portuguese",
      "fr" -> "french", "zh" -> "chinese", "de" -> "german", "ru" -> "russian",
      "jv" -> "japanese", "nl" -> "dutch", "ar" -> "arabic", "pl" -> "polish",
      "da" -> "danish", "it" -> "italian", "no" -> "norwegian")

  val langN = lang2.map{case (k,v) => (v,k)}

  val lcode:Option[String] = langCodeSystem match {
    case Some("ISO 639-1") => langCode.map(_.toLowerCase).
        flatMap(la => if (lang2.contains(la)) Some(la) else None)
    case _ => langDisplayName.flatMap(la => langN.get(la.toLowerCase))
  }

  val role2 = role match {
    case Some("PAT") => Some("PAT")     // patient
    case Some("PROV") => Some("PROV")   // healthCareProvider
    case Some("PAYOR") => Some("PAYOR") // payor
    case _ => None
  }

  override def toSrcExpression(conv: MeshConverter,
                               env: Seq[SearchParameter]): Option[String] = {
//println(s"***lcode=$lcode")
    lcode.map(lc => s"(la:(%22$lc%22))")
  }

  override def getCategories: Seq[Category] = {
    Seq(
      Category("informationRecipient", role2.getOrElse("")),
      Category("informationRecipient.languageCode.c", langCode.getOrElse("")),
      Category("informationRecipient.languageCode.cs", langCodeSystem.getOrElse("")),
      Category("informationRecipient.languageCode.dn", langDisplayName.getOrElse(""))
    ).filter(!_.term.isEmpty)
  }

  override def toString =
    s"""InfoRecipient(role: Option[String] = $role,
                      langCodeSystem: Option[String] = $langCodeSystem,
                      langCode: Option[String] = $langCode,
                      langDisplayName: Option[String] = $langDisplayName)"""
}

object InfoRecipient extends Parser {
  override def parse(parameters: Map[String,String]): Option[InfoRecipient] = {
    parameters.find(_._1.startsWith("informationRecipient")) match {
      case Some(_) => Some(new InfoRecipient(
        parameters.get("informationRecipient"),
        parameters.get("informationRecipient.languageCode.c"),
        parameters.get("informationRecipient.languageCode.cs"),
        parameters.get("informationRecipient.languageCode.dn")
      ))
      case None => None
    }
  }
}
