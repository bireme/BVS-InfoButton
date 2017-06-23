package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}

class MainSearchCriteria(val code: Option[String] = None,
                         codeSystem: Option[String] = None,
                         val displayName: Option[String] = None,
                         originalText: Option[String] = None)
                                                       extends SearchParameter {
  override def tryToString(conv: MeshConverter): Option[String] = {
    val cSystem = codeSystem.getOrElse("MESH")

    code match {
      case Some(c) => conv.convert(cSystem, c) match {
        case Some (cc) => Some(s"(mh:($cc))")
        case None => None
      }
      case None => displayName match {
        case Some(dn) => Some(s"(tw:$dn)")
        case None => originalText match {
          case Some(ot) => Some(s"(tw:$ot)")
          case None => None
        }
      }
    }
  }

  override def getCategories: Seq[Category] = {
    Seq(
      Category("mainSearchCriteria.v.c",  code.getOrElse("")),
      Category("mainSearchCriteria.v.cs", codeSystem.getOrElse("2.16.840.1.113883.6.177")),
      Category("mainSearchCriteria.v.dn", displayName.getOrElse("")),
      Category("mainSearchCriteria.v.ot", originalText.getOrElse(""))
    )
  }
}
