/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter, Tools, StopWords}
import scala.collection.immutable.TreeSet
import scala.util.{Try, Success, Failure}

class MainSearchCriteria(conv: MeshConverter,
                         val code: Option[String] = None,
                         val codeSystem: Option[String] = None,
                         val displayName: Option[String] = None,
                         val originalText: Option[String] = None)
    extends SearchParameter {

  //println(s"code=$code codeSystem=$codeSystem")
  require(code.isEmpty || codeSystem.isDefined,
    s"code[$code].isEmpty || codeSystem[$codeSystem].isDefined")
  require(code.isDefined || originalText.isDefined,
    s"code[$code].isDefined || originalText[$originalText].isDefined")

  val (codeSet: Set[String], strCodeStatus: String) = code match {
    case Some(cod) => conv.convert(codeSystem.get, cod) match {
      case Right(cod2) => (Set[String](cod2), "exact")
      case Left(descr) => descr match {
        case Some(des: Set[String]) => (des, "synonym")
        case None => (Set[String](), "not found")
      }
    }
    case None => (Set[String](), "not found")
  }
//println(s"code=$code codeSet=$codeSet strCodeStatus=$strCodeStatus")

  //val strDispName = displayName.getOrElse("").trim
  lazy val (displayNameSet: Set[String], strDisplayNameStatus: String) = displayName match {
    case Some(dName) =>
      conv.convert(codeSystem.get, dName) match {
        case Right(cod) => (Set[String](cod.trim), "exact")
        case Left(descr) => descr match {
          case Some(des) => (des, "synonym")
          case None => (Set[String](), "not found")
        }
      }
    case None => (Set[String](), "not found")
  }
//println(s"displayName=$displayName displayNameSet=$displayNameSet strDisplayNameStatus=$strDisplayNameStatus")

  lazy val (strOriginalText, strOriginalTextStatus) =
    (originalText.getOrElse("").trim, "original")
//println(s"strOriginalText=$strOriginalText strOriginalTextStatus=$strOriginalTextStatus")

  override def toSrcExpression(env: Seq[SearchParameter]): Option[String] = {
    val ret = code match {
      case Some(_) =>
        strCodeStatus match {
          case "exact"  => Some("(mh:\"" + codeSet + "\" OR ti:\"" + codeSet +
                                 "\" OR ab:\"" + codeSet + "\")")
          case "synonym"  => Some("(" +
                                  andOrExpression("mh", codeSet) + " OR " +
                                  andOrExpression("ti", codeSet) + " OR " +
                                  andOrExpression("ab", codeSet) + ")")
          case _ => if (displayNameSet.isEmpty) {
            if (strOriginalText.isEmpty) {
              None
            } else {
              Some("(" +
                andOrExpression("mh", strOriginalText) + " OR " +
                andOrExpression("ti", strOriginalText) + " OR " +
                andOrExpression("ab", strOriginalText) + ")"
              )
            }
          } else {
            Some("(" +
              andOrExpression("mh", displayNameSet) + " OR " +
              andOrExpression("ti", displayNameSet) + " OR " +
              andOrExpression("ab", displayNameSet) + ")"
            )
          }
        }
      case None =>
        if (strOriginalText.isEmpty) {
          None
        } else {
          Some("(" +
            andOrExpression("mh", strOriginalText) + " OR " +
            andOrExpression("ti", strOriginalText) + " OR " +
            andOrExpression("ab", strOriginalText) + ")"
          )
        }
    }
    ret
  }

  private def andOrExpression(index: String,
                              inSet: Set[String]): String = {
    val builder = new StringBuilder("(")

    inSet.map(x => andOrExpression(index, x)).toSeq.zipWithIndex.foreach {
      case (descr, idx) =>
        if (idx > 0) builder.append(" OR ")
        builder.append(descr)
    }
    builder.append(")").toString()
  }

  private def andOrExpression(index: String,
                              in: String): String = {
    require (index != null)
    require (in != null)

    val words = in.trim.split("\\s+").foldLeft[Set[String]](TreeSet()) {
      case (set, word) =>
        val normWord = Tools.uniformString(word)
        if ((normWord.length > 2) && (!StopWords.stopwords.contains(normWord))) {
          set + normWord
        } else {
          set
        }
    }

    val words2 = words.map(word => s"$index:$word")
    val words3 = if (words2.size > 5) {
      words2.mkString(" OR ")
    } else {
      words2.mkString(" AND ")
    }

    s"($words3)"
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

    if (index == 0) {
      getCategories
    } else {
      Seq(
        Category(s"mainSearchCriteria.v.c$index", code.getOrElse("")),
        Category(
          s"mainSearchCriteria.v.cs$index",
          codeSystem.getOrElse("2.16.840.1.113883.6.177")
        ),
        Category(s"mainSearchCriteria.v.dn$index", displayName.getOrElse("")),
        Category(s"mainSearchCriteria.v.ot$index", originalText.getOrElse(""))
      ).filter(!_.term.isEmpty)
    }
  }

  override def toString: String =
    s"""MainSearchCriteria(val code: Option[String] = $code,
                           codeSystem: Option[String] = $codeSystem,
                           val displayName: Option[String] = $displayName,
                           originalText: Option[String] = $originalText)"""
}

object MainSearchCriteria extends Parser {
  override def parse(conv: MeshConverter,
                     parameters: Map[String, String])
    : (Seq[SearchParameter], Map[String, String]) = {
    val (msc, others) =
      parameters.partition(_._1.startsWith("mainSearchCriteria.v."))
//println(s"parameters=$parameters msc=$msc others=$others")
    (getMSC(conv, msc, 0, Seq()), others)
  }

  private def getMSC(conv: MeshConverter,
                     msc: Map[String, String],
                     cardinality: Int,
                     auxSeq: Seq[MainSearchCriteria]): Seq[SearchParameter] = {
    val cardi = if ((cardinality == 0) &&
      msc.exists(p => p._1 matches "mainSearchCriteria.v.(c|dn|ot)" )) {
      ""
    } else {
      cardinality.toString
    }
    val (mscCard, other) = msc.partition(
      p => p._1 matches s"mainSearchCriteria.v.(c|cs|dn|ot)$cardi")

    if (mscCard.isEmpty) {
      auxSeq
    } else {
      val newAuxSeq = Try (
        new MainSearchCriteria(
          conv,
          mscCard.get(s"mainSearchCriteria.v.c$cardi"),
          mscCard.get(s"mainSearchCriteria.v.cs$cardi"),
          mscCard.get(s"mainSearchCriteria.v.dn$cardi"),
          mscCard.get(s"mainSearchCriteria.v.ot$cardi")
        )
      ) match {
        case Success(s) => auxSeq :+ s
        case Failure(_) => auxSeq
      }
//println(s"newAuxSeq=${newAuxSeq}")
      getMSC(conv, other, cardinality + 1, newAuxSeq)
    }
  }
}
