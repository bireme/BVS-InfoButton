package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}

class InfoRecipientPat(langCode: Option[String] = None,
                       langCodeSystem: Option[String] = None,
                       langDisplayName: Option[String] = None)
                                 extends InfoRecipient(langCodeSystem, langCode,
                                                              langDisplayName) {
  override def tryToString(conv: MeshConverter): Option[String] = {
println(s"***lcode=$lcode")
    lcode match {
      case Some(lc) => Some(s"%20AND%20(la:(%22$lc%22))")
      case None => None
    }
  }

  override def getCategories: Seq[Category] = {
    Seq(
      Category("informationRecipient", "PAT"),
      Category("informationRecipient.languageCode.c", langCode.getOrElse("")),
      Category("informationRecipient.languageCode.cs", langCodeSystem.getOrElse("")),
      Category("informationRecipient.languageCode.dn", langDisplayName.getOrElse(""))
    )
  }
}
