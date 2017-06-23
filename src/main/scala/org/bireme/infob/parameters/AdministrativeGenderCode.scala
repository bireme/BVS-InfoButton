package org.bireme.infob.parameters

import org.bireme.infob.{Category,MeshConverter}

class AdministrativeGenderCode(code: Option[String] = None,
                               codeSystem: Option[String] = None,
                               displayName: Option[String] = None)
                                                       extends SearchParameter {
  val csystem = codeSystem.getOrElse("AdministrativeGender")
  require (csystem.equals("AdministrativeGender") ||
           csystem.equals("2.16.840.1.113883.1.11.1"))

  require (!code.isEmpty || !displayName.isEmpty)

  val agcode = code.map(_.toLowerCase) match {
    case Some("f") => Some("female")
    case Some("m") => Some("male")
    case Some("un") => None  // Sugestão do RTM
    case _ => displayName.map(_.toLowerCase) match {
      case Some("female") => Some("female")
      case Some("male") => Some("male")
      case Some("undifferentiated") => None
      case _ => None
    }
  }

  override def tryToString(conv: MeshConverter): Option[String] = {
    agcode match {
      case Some(agc) => Some(s"%20AND%20(limit:(%22$agc%22))")
      case None => None
    }
  }

  override def getCategories: Seq[Category] = {
    Seq(
      Category("administrativeGenderCode.v.c",  code.getOrElse("")),
      Category("administrativeGenderCode.v.cs", codeSystem.getOrElse("")),
      Category("administrativeGenderCode.v.dn", displayName.getOrElse(""))
    )
  }
}
