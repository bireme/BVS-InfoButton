/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

import org.bireme.infob.parameters.{MainSearchCriteria, SearchParameter}
import org.dom4j.Element
import org.dom4j.dom.DOMElement
import play.api.libs.json._

//List((source,bvs_infobutton), (start,0), (rows,10), (sort,da desc), (wt,json),
//(q,(mh:"Asthenia" OR ti:"Asthenia" OR ab:"Asthenia") AND (instance:"regional") AND (fulltext:"1") AND (pais_assunto:"brasil")))
object Explainer {
  def explain(info: InfoServer,
              srcParam: Seq[SearchParameter],
              expression: Option[Seq[(String, String)]],
              isXml: Boolean = true): Either[JsObject, Element] = {
    if (isXml) Right(explainXml(info, srcParam, expression))
    else Left(explainJson(info, srcParam, expression))
  }

  def explainJson(info: InfoServer,
                  srcParam: Seq[SearchParameter],
                  expression: Option[Seq[(String, String)]]) : JsObject = {
    val res = expression match {
      case Some(expr) => parse(info, expr)
      case None => Seq(("Search expression or term not found",
                        Seq[(String, Int)]()))
    }
    val terms = JsArray(
      convert(srcParam) map {
        param =>
          JsObject(Seq("original" -> JsString(param._1),
                       "after" -> JsString(param._2),
                       "status" -> JsString(param._3),
                       "thesaurus" -> JsString(param._4.getOrElse(""))))
      }
    )
    val explanation = JsArray(res.map {
      x =>
        val lst = x._2 map {
          y => JsObject(Seq(y._1 -> JsNumber(y._2)))
        }
        JsObject(Seq("expression" -> JsString(x._1), "documents" -> JsArray(lst)))
    })
    JsObject(Seq("explain" -> JsObject(Seq("terms"-> terms,
                                           "explanation"-> explanation))))
  }

  def explainXml(info: InfoServer,
                 srcParam: Seq[SearchParameter],
                 expression: Option[Seq[(String, String)]]) : Element = {
    val res = expression match {
      case Some(expr) => parse(info, expr)
      case None => Seq(("Search expression or term not found",
                   Seq[(String, Int)]()))
    }
    val element = new DOMElement("explain")
    val terms = element.addElement("terms")

    convert(srcParam).foreach {
      param =>
        val thesaurus = terms.addElement("term")
        thesaurus.addElement("original").addText(param._1)
        thesaurus.addElement("after").addText(param._2).addAttribute("status", param._3)
        thesaurus.addElement("thesaurus").addText(param._4.getOrElse(""))
    }

    res.foreach {
      x =>
        val explanation = element.addElement("explanation")
        explanation.addElement("expression").addText(x._1)
        val documents = explanation.addElement("documents")
        x._2.foreach(y => documents.addElement(y._1).addText(y._2.toString))
    }
    element
  }

  // (original, after, status, codeSystem)
  private def convert(srcParam: Seq[SearchParameter]):
                              Seq[(String, String, String, Option[String])] = {
    require(srcParam != null)
//println(s"convert srcParam=$srcParam")
    srcParam.filter(_.isInstanceOf[MainSearchCriteria]).map {
      param =>
        val msc = param.asInstanceOf[MainSearchCriteria]
        if (msc.strCode.isEmpty) {
          if (msc.strDisplayName.isEmpty) {
            if (msc.originalText.isEmpty) {
              (msc.code.getOrElse(""), msc.strCode,
               msc.strCodeStatus, msc.codeSystem)
            } else (msc.originalText.get, msc.strOriginalText,
                    msc.strOriginalTextStatus, msc.codeSystem)
          } else (msc.displayName.get, msc.strDisplayName,
                  msc.strDisplayNameStatus, msc.codeSystem)
        } else (msc.code.get, msc.strCode, msc.strCodeStatus, msc.codeSystem)
    }
  }

  private def parse(info: InfoServer,
                    expr: Seq[(String, String)]):
                                           Seq[(String, Seq[(String, Int)])] = {
    val (bexpr, others) = expr.partition(_._1 equals("q"))
//println(s"bexpr=$bexpr others=$others")
    if (bexpr.isEmpty) Seq(("No search expression found",
                             info.studies.map(x => (x, 0))))
    else {
      val bex = bexpr.head._2
//println(s"bex=$bex")
      val split = bex.split("(?<=\\)) AND (?=\\()").toSeq
//println(s"split=$split")
      val (instFull, others1) = split.partition(x =>
         x.startsWith("(instance:") || x.startsWith("(fulltext:"))
//println(s"instFull=$instFull, others1=$others1")
      val (mainSrcCrit, others2) = others1.partition(_.contains("(mh:"))
//println(s"mainSrcCrit=$mainSrcCrit, others2=$others2")

      parse2(info, mainSrcCrit, instFull, others2, others)
    }
  }

  private def parse2(info: InfoServer,
                     mainSrcCrit: Seq[String],
                     instFull: Seq[String],
                     others: Seq[String],
                     expr: Seq[(String, String)]): // (expr, (tipoDoc,numDoc))
                                           Seq[(String, Seq[(String, Int)])] = {
    // Test each MainSearchCriteria alone
    val seq1 = mainSrcCrit.foldLeft[Seq[(String, Seq[(String, Int)])]] (Seq()) {
      case (seq, msc) =>
        val queryQ = createQ(Seq(msc), instFull, Seq())
        val nod = numOfDocs(info, expr :+ queryQ)
//println(s"queryQ=$queryQ nod=$nod")
        seq :+ (queryQ._2, nod)
    }
    if (isEmpty(seq1)) seq1 // if no one found documents
    else {
      // Test with all MainSearchCriteria at same time
//println(s"mainSrcCrit.size=${mainSrcCrit.size})")
      val seq3 = if (mainSrcCrit.size == 1) seq1
        else {
          val seq2 = {
            val queryQ = createQ(mainSrcCrit, instFull, Seq())
            val nod = numOfDocs(info, expr :+ queryQ)
            Seq((queryQ._2, nod))
          }
          seq1 ++ seq2
        }
      // Test all MainSearchCriteria with each other parameter alone
      val seq4 = others.foldLeft[Seq[(String, Seq[(String, Int)])]] (Seq()) {
        case (seq, other) =>
          val queryQ = createQ(mainSrcCrit, instFull, Seq(other))
          val nod = numOfDocs(info, expr :+ queryQ)
          seq :+ (queryQ._2, nod)
      }
      val seq5 = seq3 ++ seq4
      if (others.size <= 1) seq5
      else {
        // Test with all other parameters at same time
        val queryQ = createQ(mainSrcCrit, instFull, others)
        val nod = numOfDocs(info, expr :+ queryQ)
        seq5 :+ (queryQ._2, nod)
      }
    }
  }

  private def createQ(mainSrcCrit: Seq[String],
                      instFull: Seq[String],
                      other: Seq[String]): (String, String) = {
    val bool = mainSrcCrit.mkString(" AND ") +
    (if (instFull.isEmpty) "" else s""" AND ${instFull.mkString(" AND ")}""") +
    (if (other.isEmpty) "" else s""" AND ${other.mkString(" AND ")}""")

    //("q", bool.replace('"', '\''))
    ("q", bool)
  }

  private def numOfDocs(info: InfoServer,
                        expr: Seq[(String, String)]): Seq[(String, Int)] = {
    val seq = info.orderedSearch(info.studies, Some(expr), 10)

    info.studies map(tos => (tos, seq.count(_._1 equals tos)))
  }

  private def isEmpty(seq: Seq[(String, Seq[(String, Int)])]): Boolean =
    seq.exists(x => isEmpty2(x._2))

  private def isEmpty2(seq: Seq[(String, Int)]): Boolean =
    !seq.exists(_._2 > 0)
}
