/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}

class Performer(role: Option[String],
                langCode: Option[String] = None,
                langCodeSystem: Option[String] = None,
                langDisplayName: Option[String] = None) extends SearchParameter {
  val lang2 = Map("en" -> "english", "es" -> "spanish", "pt" -> "portuguese",
    "fr" -> "french", "zh" -> "chinese", "de" -> "german", "ru" -> "russian",
    "jv" -> "japanese", "nl" -> "dutch", "ar" -> "arabic", "pl" -> "polish",
    "da" -> "danish", "it" -> "italian", "no" -> "norwegian")
  val langN = lang2.map{case (k,v) => (v,k)}

  val lcode = langCodeSystem match {
    case Some("ISO 639-1") =>
      langCode.map(_.toLowerCase) match {
        case Some(la) => if (lang2.contains(la)) Some(la) else None
        case None => None
      }
    case _ => langDisplayName.map(_.toLowerCase) match {
      case Some(la) => lang2.get(la)
      case None => langCode.map(_.toLowerCase) match {
        case Some(la) => if (lang2.contains(la)) Some(la) else None
        case None => None
      }
    }
  }

  val role2 = role match {
    case Some("PAT") => Some("PAT")
    case Some("PROV") => Some("PROV")
    case Some("PAYOR") => Some("PAYOR")
    case _ => None
  }

  override def toSrcExpression(conv: MeshConverter): Option[String] = {
//println(s"***lcode=$lcode")
    lcode match {
      case Some(lc) => Some(s"%20AND%20(la:(%22$lc%22))")
      case None => None
    }
  }

  override def getCategories: Seq[Category] = {
    Seq(
      Category("performer", role2.getOrElse("")),
      Category("performer.languageCode.c", langCode.getOrElse("")),
      Category("performer.languageCode.cs", langCodeSystem.getOrElse("")),
      Category("performer.languageCode.dn", langDisplayName.getOrElse(""))
    ).filter(!_.term.isEmpty)
  }

  override def toString =
    s"""Performer(role: Option[String] = $role,
                  langCode: Option[String] = $langCode,
                  langCodeSystem: Option[String] = $langCodeSystem,
                  langDisplayName: Option[String] = $langDisplayName)"""
}

object Performer extends Parser {
  override def parse(parameters: Map[String,String]): Option[Performer] = {
    parameters.find(_._1.startsWith("performer")) match {
      case Some(_) => Some(new Performer(
        parameters.get("performer"),
        parameters.get("performer.languageCode.c"),
        parameters.get("performer.languageCode.cs"),
        parameters.get("performer.languageCode.dn")
      ))
      case None => None
    }
  }
}
