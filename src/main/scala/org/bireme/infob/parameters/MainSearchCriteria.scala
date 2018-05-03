/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}
import scala.util.{Try, Success, Failure}

class MainSearchCriteria(val code: Option[String] = None,
                         codeSystem: Option[String] = None,
                         val displayName: Option[String] = None,
                         val originalText: Option[String] = None)
    extends SearchParameter {
  assert((!code.isEmpty) || (!displayName.isEmpty) || (!originalText.isEmpty))

  override def toSrcExpression(conv: MeshConverter,
                               env: Seq[SearchParameter]): Option[String] = {
    val cSystem = codeSystem.getOrElse("2.16.840.1.113883.6.177") // MESH
    val strCode = conv.convert(cSystem, code.getOrElse("")) match {
      case Right(cod) => cod.trim
      case Left(descr) => descr match {
        case Some(des) => des.trim
        case None => ""
      }
    }
//println(s"strCode=$strCode")
    //val strDispName = displayName.getOrElse("").trim
    val strDisplayName = conv.convert(cSystem, displayName.getOrElse("")) match {
      case Right(cod) => cod.trim
      case Left(descr) => descr match {
        case Some(des) => des.trim
        case None => ""
      }
    }

    val strOriginalText = originalText.getOrElse("").trim

    if (strCode.isEmpty) {
      if (strDisplayName.isEmpty) {
        if (strOriginalText.isEmpty) None else
          Some(s"(mh:($strOriginalText) OR ti:($strOriginalText) OR ab:($strOriginalText))")
      } else Some(s"(mh:${'"'}$strDisplayName${'"'} OR ti:${'"'}$strDisplayName${'"'} OR ab:${'"'}$strDisplayName${'"'})")
    } else Some(s"(mh:${'"'}$strCode${'"'} OR ti:${'"'}$strCode${'"'} OR ab:${'"'}$strCode${'"'})")
  }

  override def getCategories: Seq[Category] = {
    Seq(
      Category("mainSearchCriteria.v.c", code.getOrElse("")),
      Category("mainSearchCriteria.v.cs",
               codeSystem.getOrElse("2.16.840.1.113883.6.177")),
      Category("mainSearchCriteria.v.dn", displayName.getOrElse("")),
      Category("mainSearchCriteria.v.ot", originalText.getOrElse(""))
    ).filter(!_.term.isEmpty)
  }

  def getCategories(index: Int): Seq[Category] = {
    require(index >= 0)

    if (index == 0) getCategories
    else
      Seq(
        Category(s"mainSearchCriteria.v.c$index", code.getOrElse("")),
        Category(s"mainSearchCriteria.v.cs$index",
                 codeSystem.getOrElse("2.16.840.1.113883.6.177")),
        Category(s"mainSearchCriteria.v.dn$index", displayName.getOrElse("")),
        Category(s"mainSearchCriteria.v.ot$index", originalText.getOrElse(""))
      ).filter(!_.term.isEmpty)
  }

  override def toString =
    s"""MainSearchCriteria(val code: Option[String] = $code,
                           codeSystem: Option[String] = $codeSystem,
                           val displayName: Option[String] = $displayName,
                           originalText: Option[String] = $originalText)"""
}

object MainSearchCriteria extends Parser {
  override def parse(parameters: Map[String, String])
    : (Seq[SearchParameter], Map[String, String]) = {
    val (msc, others) =
      parameters.partition(_._1.startsWith("mainSearchCriteria.v."))
//println(s"parameters=$parameters msc=$msc others=$others")
    (getMSC(msc, 0, Seq()), others)
  }

  private def getMSC(msc: Map[String, String],
                     cardinality: Int,
                     auxSeq: Seq[MainSearchCriteria]): Seq[SearchParameter] = {
//println(s"*** getMSC msc=$msc cardinality=$cardinality auxSeq=$auxSeq")
    val cardi = if ((cardinality == 0) &&
      (msc.exists(p => p._1 matches("mainSearchCriteria.v.(c|cs|dn|ot)")))) ""
      else cardinality.toString
    val (mscCard, other) = msc.partition(
      p => p._1 matches s"mainSearchCriteria.v.(c|cs|dn|ot)$cardi")

    if (mscCard.isEmpty) auxSeq
    else {
      val newAuxSeq = Try (
        new MainSearchCriteria(
          mscCard.get(s"mainSearchCriteria.v.c$cardi"),
          mscCard.get(s"mainSearchCriteria.v.cs$cardi"),
          mscCard.get(s"mainSearchCriteria.v.dn$cardi"),
          mscCard.get(s"mainSearchCriteria.v.ot$cardi")
        )
      ) match {
        case Success(s) => auxSeq :+ s
        case Failure(_) => auxSeq
      }
      getMSC(other, cardinality + 1, newAuxSeq)
    }
  }
}
