/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}
import scala.util.{Try, Success, Failure}

class Performer(role: Option[String],
                langCode: Option[String] = None,
                langCodeSystem: Option[String] = None,
                langDisplayName: Option[String] = None) extends SearchParameter {
  val lang2 = ISO639_1_Codes.codes.map { case (k, v) => (k, v.head) }
  val langN = lang2.map { case (k, v) => (v, k) }

  val lcode = langCodeSystem match {
    case Some("ISO 639-1") =>
      langCode
        .map(_.toLowerCase)
        .flatMap(la => if (lang2.contains(la)) Some(la) else None)
    case _ => langDisplayName.flatMap(la => langN.get(la.toLowerCase))
  }

  val role2 = role match {
    case Some("PAT")   => Some("PAT")
    case Some("PROV")  => Some("PROV")
    case Some("PAYOR") => Some("PAYOR")
    case _             => None
  }

  override def toSrcExpression(conv: MeshConverter,
                               env: Seq[SearchParameter]): Option[String] = {
//println(s"***lcode=$lcode")
    env.collectFirst({ case ir: InfoRecipient => ir }) match {
      case Some(ir: InfoRecipient) =>
        ir.lcode match {
          case Some(lang) => Some(s"(la:(%22$lang%22))")
          case _          => lcode.map(lc => s"(la:(%22$lc%22))")
        }
      case _ => lcode.map(lc => s"(la:(%22$lc%22))")
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
  override def parse(parameters: Map[String, String])
    :(Seq[SearchParameter], Map[String, String]) = {

    val (per, others) = parameters.partition(_._1.startsWith("performer"))

    if (per.isEmpty) (Seq(), others)
    else {
      Try (
        new Performer(
          parameters.get("performer"),
          parameters.get("performer.languageCode.c"),
          parameters.get("performer.languageCode.cs"),
          parameters.get("performer.languageCode.dn")
        )
      ) match {
        case Success(s) => (Seq(s), others)
        case Failure(_) => (Seq(), others)
      }
    }
  }
}
