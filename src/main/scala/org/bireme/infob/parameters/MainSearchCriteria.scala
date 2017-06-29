package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}

class MainSearchCriteria(val code: Option[String] = None,
                         codeSystem: Option[String] = None,
                         val displayName: Option[String] = None,
                         originalText: Option[String] = None)
                                                       extends SearchParameter {
  override def toSrcExpression(conv: MeshConverter): Option[String] = {
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
    ).filter(!_.term.isEmpty)
  }

  override def toString =
    s"""MainSearchCriteria(val code: Option[String] = $code,
                           codeSystem: Option[String] = $codeSystem,
                           val displayName: Option[String] = $displayName,
                           originalText: Option[String] = $originalText)"""
}

object MainSearchCriteria {
  def parse(parameters: Map[String,String]): (Seq[MainSearchCriteria],
                                              Map[String,String]) = {
    val (msc,other) = parameters.partition(_._1.startsWith("mainSearchCriteria.v."))

    (getMSC(msc, 0, Seq()), other)
  }

  private def getMSC(msc: Map[String,String],
                     cardinality: Int,
                     auxSeq: Seq[MainSearchCriteria]): Seq[MainSearchCriteria] = {
    val cardi = if (cardinality == 0) "" else cardinality.toString
    val (mscCard,other) = msc.partition(p => p._1 matches
                                     s"mainSearchCriteria.v.(c|cs|dn|ot)$cardi")
    if (mscCard.isEmpty) auxSeq
    else getMSC(other,
                cardinality + 1,
                auxSeq :+ (new MainSearchCriteria(
                  mscCard.get(s"mainSearchCriteria.v.c$cardi"),
                  mscCard.get(s"mainSearchCriteria.v.cs$cardi"),
                  mscCard.get(s"mainSearchCriteria.v.dn$cardi"),
                  mscCard.get(s"mainSearchCriteria.v.ot$cardi"))))
  }
}
