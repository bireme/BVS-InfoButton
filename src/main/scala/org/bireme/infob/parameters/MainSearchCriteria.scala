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
                         originalText: Option[String] = None)
    extends SearchParameter {
  assert((!code.isEmpty) || (!displayName.isEmpty) || (!originalText.isEmpty))

  private def replaceSpaces(in: String): String =
    if (in == null) null else in.replace(" ", "%20")

  override def toSrcExpression(conv: MeshConverter,
                               env: Seq[SearchParameter]): Option[String] = {
    val cSystem = codeSystem.getOrElse("2.16.840.1.113883.6.177") // MESH

    code match {
      case Some(c) =>
        /*println("vai converter");*/
        conv.convert(cSystem, c) match {
          case Right(cod) =>
            Some(s"(mh:(%22${replaceSpaces(cod)}%22))")
          case Left(descr) =>
            descr match {
              case Some(des) => Some(s"(ti:%22${replaceSpaces(des)}%22)")
              case None =>
                displayName match {
                  case Some(dn) => Some(s"(ti:%22${replaceSpaces(dn)}%22)")
                  case None =>
                    originalText.map(ot => s"(ti:%22${replaceSpaces(ot)}%22)")
                }
            }
        }
      case None =>
        displayName match {
          case Some(dn) => Some(s"(mh:%22${replaceSpaces(dn)}%22)")
          case None     =>
            /*println("original text =>");*/
            originalText.map(ot => s"(ti:%22${replaceSpaces(ot)}%22)")
        }
    }
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

    (getMSC(msc, 0, Seq()), others)
  }

  private def getMSC(msc: Map[String, String],
                     cardinality: Int,
                     auxSeq: Seq[MainSearchCriteria]): Seq[SearchParameter] = {
//println(s"*** getMSC msc=$msc cardinality=$cardinality auxSeq=$auxSeq")

    val cardi = if (cardinality == 0) "" else cardinality.toString
    val (mscCard, other) = msc.partition(
      p =>
        p._1 matches
          s"mainSearchCriteria.v.(c|cs|dn|ot)$cardi")
    if (mscCard.isEmpty) auxSeq
    else {
      val newAuxSeq = Try (
        new MainSearchCriteria(
          mscCard.get(s"mainSearchCriteria.v.c$cardi"),
          mscCard.get(s"mainSearchCriteria.v.cs$cardi"),
          mscCard.get(s"mainSearchCriteria.v.dn$cardi"),
          mscCard.get(s"mainSearchCriteria.v.ot$cardi")
        )) match {
          case Success(s) => auxSeq :+ s
          case Failure(_) => auxSeq
        }
      getMSC(other, cardinality + 1, newAuxSeq)
    }
  }
}
