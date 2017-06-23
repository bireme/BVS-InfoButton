package org.bireme.infob.parameters

abstract class InfoRecipient(langCodeSystem: Option[String] = None,
                             langCode: Option[String] = None,
                             langDisplayName: Option[String] = None)
                                                       extends SearchParameter {
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
}
