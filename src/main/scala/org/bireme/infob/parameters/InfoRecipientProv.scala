package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}

class InfoRecipientProv(code: Option[String] = None,
                        codeSystem: Option[String] = None,
                        displayName: Option[String] = None,
                        langCode: Option[String] = None,
                        langCodeSystem: Option[String] = None,
                        langDisplayName: Option[String] = None)
                                 extends InfoRecipient(langCodeSystem, langCode,
                                                              langDisplayName) {
  override def tryToString(conv: MeshConverter): Option[String] = {
    lcode match {
      case Some(lc) => Some(s"%20AND%20(la:(%22$lc%22))")
      case None => None
    }
  }

  override def getCategories: Seq[Category] = {
    Seq(
      Category("informationRecipient", "PROV"),
      Category("informationRecipient.healthCareProvider.c.c", code.getOrElse("")),
      Category("informationRecipient.healthCareProvider.c.cs", codeSystem.getOrElse("")),
      Category("informationRecipient.healthCareProvider.c.dn", displayName.getOrElse("")),
      Category("informationRecipient.languageCode.c", langCode.getOrElse("")),
      Category("informationRecipient.languageCode.cs", langCodeSystem.getOrElse("")),
      Category("informationRecipient.languageCode.dn", langDisplayName.getOrElse(""))
    )
  }
}
