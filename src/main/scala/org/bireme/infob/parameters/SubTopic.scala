/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}

case class SubTopic(
    codeSystem: Option[String] = Some("2.16.840.1.113883.6.177"),
    code: Option[String] = None,
    displayName: Option[String] = None,
    originalText: Option[String] = None)
    extends SearchParameter {
  assert(
    (codeSystem != null) && (code != null) &&
      (displayName != null) && (originalText != null))
  assert(
    codeSystem.equals("2.16.840.1.113883.6.177") || // MeSH
      codeSystem.equals("2.16.840.1.113883.6.96")) // SNOMED CT
  assert((!code.isEmpty) || (!displayName.isEmpty) || (!originalText.isEmpty))

  private def replaceSpaces(in: String): String =
    if (in == null) null else in.replace(" ", "%20")

  private def slashText(text: String): String = {
    val ttrim = replaceSpaces(text.trim)
    if (ttrim(0) == '/') ttrim else s"/$ttrim"
  }

  override def toSrcExpression(conv: MeshConverter,
                               env: Seq[SearchParameter]): Option[String] = {
    code match {
      case Some(c) =>
        conv.convert(codeSystem.get, c) match {
          case Right(co) =>
            Some(
              co.map(cod => s"(mh:(%22${replaceSpaces(cod)}%22))")
                .mkString("%20OR%20"))
          case Left(descr) =>
            descr match {
              case Some(des) =>
                Some(s"((ti:%22${des}%22)%20OR%20(ab:%22${des}%22))")
              case None =>
                displayName match {
                  case Some(dn) =>
                    val stext = slashText(dn)
                    Some(s"((ti:%22${stext}%22)%20OR%20(ab:%22${stext}%22))")
                  case None =>
                    originalText.map { ot =>
                      val stext = slashText(ot)
                      s"((ti:%22${stext}%22)%20OR%20(ab:%22${stext}%22))"
                    }
                }
            }
        }
      case None =>
        displayName match {
          case Some(dn) =>
            val stext = slashText(dn)
            Some(s"((ti:%22${stext}%22)%20OR%20(ab:%22${stext}%22))")
          case None =>
            originalText.map { ot =>
              val stext = slashText(ot)
              s"((ti:%22${stext}%22)%20OR%20(ab:%22${stext}%22))"
            }
        }
    }
  }

  override def getCategories: Seq[Category] = {
    Seq(
      Category("subTopic.v.c", code.getOrElse("")),
      Category("subTopic.v.cs", codeSystem.get),
      Category("subTopic.v.dn", displayName.getOrElse("")),
      Category("subTopic.v.ot", originalText.getOrElse(""))
    ).filter(!_.term.isEmpty)
  }

  override def toString =
    s"""subTopic(codeSystem: Option[String] = $codeSystem,
                 code: Option[String] = $code,
                 displayName: Option[String] = $displayName,
                 originalText: Option[String] = $originalText)"""
}
