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
  require(code.isEmpty || !codeSystem.isEmpty)
  require(!code.isEmpty || !originalText.isEmpty)

  val (strCode, strCodeStatus) = code match {
    case Some(cod) => conv.convert(codeSystem.get, cod) match {
      case Right(cod) => (cod.trim, "exact")
      case Left(descr) => descr match {
        case Some(des) => (des.trim, "synonym")
        case None => ("", "not found")
      }
    }
    case None => ("", "not found")
  }
println(s"code=$code strCode=$strCode strCodeStatus=$strCodeStatus")

  //val strDispName = displayName.getOrElse("").trim
  lazy val (strDisplayName, strDisplayNameStatus) = displayName match {
    case Some(dName) =>
      conv.convert(codeSystem.get, dName) match {
        case Right(cod) => (cod.trim, "exact")
        case Left(descr) => descr match {
          case Some(des) => (des.trim, "synonym")
          case None => ("", "not found")
        }
      }
    case None => ("", "not found")
  }
println(s"displayName=$displayName strDisplayName=$strDisplayName strDisplayNameStatus=$strDisplayNameStatus")

  lazy val (strOriginalText, strOriginalTextStatus) =
    (originalText.getOrElse("").trim, "original")
println(s"strOriginalText=$strOriginalText strOriginalTextStatus=$strOriginalTextStatus")

  override def toSrcExpression(env: Seq[SearchParameter]): Option[String] = {
println(s"env=$env")
    val ret = code match {
      case Some(_) =>
        strCodeStatus match {
          case "exact"  => Some("(mh:\"" + strCode + "\" OR ti:\"" + strCode +
                                 "\" OR ab:\"" + strCode + "\")")
          case "synonym"  => Some("(" +
                                  andOrExpression("mh", strCode) + " OR " +
                                  andOrExpression("ti", strCode) + " OR " +
                                  andOrExpression("ab", strCode) + ")"
                                 )
          case _ => if (strDisplayName.isEmpty) {
            if (strOriginalText.isEmpty) None
            else Some("(" +
                   andOrExpression("mh", strOriginalText) + " OR " +
                   andOrExpression("ti", strOriginalText) + " OR " +
                   andOrExpression("ab", strOriginalText) + ")"
                  )
          } else Some("(" +
                         andOrExpression("mh", strDisplayName) + " OR " +
                         andOrExpression("ti", strDisplayName) + " OR " +
                         andOrExpression("ab", strDisplayName) + ")"
                      )
        }
      case None => if (strOriginalText.isEmpty) None
                   else Some("(" +
                          andOrExpression("mh", strOriginalText) + " OR " +
                          andOrExpression("ti", strOriginalText) + " OR " +
                          andOrExpression("ab", strOriginalText) + ")"
                         )
    }
    println(s"expr=[$ret]")
    ret

/*
    if (strCode.isEmpty) {
      if (strDisplayName.isEmpty) {
        if (strOriginalText.isEmpty) None
        else {
          val origOrExpr = OrExpression(strOriginalText)
          Some(s"(mh:($origOrExpr) OR ti:($origOrExpr) OR ab:($origOrExpr))")
        }
      } else Some(s"(mh:${'"'}$strDisplayName${'"'} OR ti:${'"'}$strDisplayName${'"'} OR ab:${'"'}$strDisplayName${'"'})")
    } else Some(s"(mh:${'"'}$strCode${'"'} OR ti:${'"'}$strCode${'"'} OR ab:${'"'}$strCode${'"'})")
    */
  }

  private def andOrExpression(index: String,
                              in: String): String = {
    require (index != null)
    require (in != null)
println(s"index=$index in=$in")
    val words = in.trim.split("\\s+").foldLeft[Set[String]](TreeSet()) {
      case (set, word) =>
        val normWord = Tools.uniformString(word)
        if ((normWord.size > 3) && (!StopWords.stopwords.contains(normWord)))
          set + normWord
        else set
    }
println(s"words=$words")
    val words2 = words.map(word => s"$index:$word")
    val words3 = if (words2.size > 5) words2.mkString(" OR ")
    else words2.mkString(" AND ")

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
//println(s"*** getMSC msc=$msc cardinality=$cardinality auxSeq=$auxSeq")
    val cardi = if ((cardinality == 0) &&
      (msc.exists(p => p._1 matches("mainSearchCriteria.v.(c|cs|dn|ot)")))) ""
      else cardinality.toString
    val (mscCard, other) = msc.partition(
      p => p._1 matches s"mainSearchCriteria.v.(c|cs|dn|ot)$cardi")
//println(s"mscCard=$mscCard other=$other")
    if (mscCard.isEmpty) auxSeq
    else {
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
        case Failure(f) => println(s"FAILURE=$f"); auxSeq
      }
//println(s"newAuxSeq=${newAuxSeq}")
      getMSC(conv, other, cardinality + 1, newAuxSeq)
    }
  }
}
