/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

import org.bireme.infob.parameters._

import ch.qos.logback.classic.{Level, Logger => xLogger}
import com.typesafe.scalalogging.Logger
import java.net.{URL, URLDecoder, URLEncoder}
import java.nio.charset.Charset
import org.slf4j.LoggerFactory
import play.api.libs.json._

import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.HttpPost
import org.apache.http.impl.client.HttpClients
import org.apache.http.message.BasicNameValuePair
import org.apache.http.util.EntityUtils

import scala.collection.JavaConverters._
import scala.reflect._
import scala.reflect.runtime.universe._
import scala.util.{Try, Success, Failure}

/**
  * This class converts an url request into a document with retrieved documents
  * from BVS that are associated with the url query parameters.
  *
  * @param conv the thesaurus to MESH term code converter
  * @param iahxURL the BVS search portal url
  *
  * @author Heitor Barbieri
  */
class InfobuttonServer(
    conv: MeshConverter,
    iahxUrl: String = "http://basalto02.bireme.br:8986/solr5/portal/select") {

  LoggerFactory
    .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
    .asInstanceOf[xLogger]
    .setLevel(Level.DEBUG) //Level.INFO

  val logger = Logger("BVS-InfoButton")

  def getInfo(param: java.util.Map[String, Array[String]], maxDocs: Int): String = {
    val pmap = param.asScala.toMap.map { case (key, value) => (key, value(0)) }
    getInfo(pmap, maxDocs)
  }

  def getInfo(url: String, maxDocs: Int): String = {
    urlToParms(url) match {
      case Some(param) => getInfo(param, maxDocs)
      case None => ""
    }
  }

  def getInfo(param: Map[String, String], maxDocs: Int = 10): String = {
    require(param != null)
    require(maxDocs > 0)
//println("Antes do ParameterParser")
    val (info, oType, callbackFunc) = ParameterParser.parse(param)
//println(s">> getInfo - param=$param  info=$info")
    val outType = oType.getOrElse("text/xml")
    val exprAND = convToSrcExpression(info, conv, maxDocs, false)
    val useORExpression = info.count(_.isInstanceOf[MainSearchCriteria]) > 1 // There are more than one MainSearchCriteria
//println(s"Antes do orderedSearch(exprAND, maxDocs) exprAND=[$exprAND]")
    val docsAND = orderedSearch(exprAND, maxDocs)
//println(s"Antes do orderedSearch(exprOR, maxDocs) remaining=$remaining")
    val exprOR = if (useORExpression) convToSrcExpression(info, conv, maxDocs, true)
      else None
    val docsOR = if (useORExpression) {
        orderedSearch(exprOR, maxDocs, Some(docsAND))
      } else Seq()
    val docs = docsAND ++ docsOR

    logger.debug((param.foldLeft[String]("\nParameters: ") {
      case (str, param) => s"$str \n\t[${param._1}: ${param._2}]"
    }))
    logger.debug((exprAND match {
      case Some(expr) => s"\nSearch expression (AND): \n\t$expr"
      case None       => "\nSearch expression (AND): Not Found"
    }))
    if (useORExpression) logger.debug((exprOR match {
      case Some(expr) => s"\nSearch expression (OR): \n\t$expr"
      case None       => "\nSearch expression (OR): Not Found."
    }))
    logger.debug("\nDocuments found:\n\t (AND) - " + docsAND.size)
    if (useORExpression) logger.debug("\n\t (OR) - " + docsOR.size)

    convToInfoResponse(info, docs, outType, callbackFunc)
  }

//http://basalto02.bireme.br:8986/solr5/portal/select?
//q=tw:((instance:%22regional%22)%20AND%20(%20mh:(c02.081.270)))&wt=json&
//indent=true&start=0&rows=10&sort=da+desc
  private def convToSrcExpression(info: Seq[SearchParameter],
                                  conv: MeshConverter,
                                  maxDocs: Int,
                                  useOR: Boolean): Option[Seq[(String, String)]] = {
    // Processing MainSearchCriteria
    val (s1,s2) = partition[MainSearchCriteria](info)
//println(s"info=$info s1=$s1 s2=$s2")
    if (s1.isEmpty)
      throw new IllegalArgumentException("missing MainSearchCriteria")
//println(s"srcParam2SrcExpr(s1, conv, useOR)=${srcParam2SrcExpr(s1, conv, useOR)}")
    srcParam2SrcExpr(s1, conv, useOR) map {
      msc_str =>
        // Processing LocationOfInterest
        val (s3,s4) = partition[LocationOfInterest](s2)
        val loi = srcParam2SrcExpr(s3, conv, true) match {
          case Some(src) => s" AND $src"
          case None => ""
        }
        println(s"*=>info=${info} s1=$s1 s3=$s3 s4=$s4")

        //Processing other parameters
        val srcParam = s4.foldLeft[String](s"$msc_str AND " +
          "(instance:\"regional\") AND (fulltext:\"1\")" + loi) {
            case (str, sparam) =>
              str +
                (sparam.toSrcExpression(conv, info) match {
                  case Some(str2) => s" AND $str2"
                  case None       => ""
                })
          }
        Seq("source" -> "bvs_infobutton",
            "start" -> "0",
            "rows" -> maxDocs.toString,
            "sort" -> "da desc",
            "wt" -> "json",
            "q" -> srcParam)
    }
  }

  /**
    * Given an input sequence of search parameters, returns two sequences:
    * a sequence of SearchParameters of subtype T and another sequence with
    * the other SearchParameters
    *
    * @param info the input sequence of SearchParameters
    * @param ev see: https://stackoverflow.com/questions/29886246/scala-filter-by-type
    * @return the resulting two sequences of SearchParameters
    */
  private def partition[T <: SearchParameter](info: Seq[SearchParameter])
                                             (implicit ev: ClassTag[T]):
                                              (Seq[T], Seq[SearchParameter]) = {
//println(s"partition info=$info")
    info.foldLeft[(Seq[T], Seq[SearchParameter])] ((Seq(), Seq())) {
      case (seq, t) => t match {
        case tt: T => (seq._1 :+ tt, seq._2)
        case _ => (seq._1, seq._2 :+ t)
      }
    }
  }

  private def srcParam2SrcExpr[T <: SearchParameter](info: Seq[T],
                               conv: MeshConverter,
                               useOR: Boolean) : Option[String] = {
    val connector = if (useOR) "OR" else "AND"
    info
      .map(_.toSrcExpression(conv, info))
      .flatten
      .sorted
      .mkString(s" $connector ") match {
        case ""  => None
        case str => Some(str)
      }
  }

  private def orderedSearch(expression: Option[Seq[(String, String)]],
                            maxDocs: Int,
                            auxDocsAnd: Option[Seq[(String, JsValue)]] = None):
                                                      Seq[(String, JsValue)] = {
    require(expression != null)
    require(maxDocs > 0)

    // overview is present to grant that at least some document will be returned
    val typeOfStudy = Seq("guideline",
                          "systematic_reviews",
                          "clinical_trials",
                          "cohort",
                          "case_control",
                          "case_reports",
                          "overview",
                          "question_answer")
    expression match {
      case Some(params) => oSearch(typeOfStudy, params, maxDocs, Seq(), auxDocsAnd)
      case None         => Seq()
    }
  }

  private def oSearch(typeOfStudy: Seq[String],
                      params: Seq[(String,String)],
                      maxDocs: Int,
                      auxSeq: Seq[(String, JsValue)],
                      auxDocsAnd: Option[Seq[(String, JsValue)]]):
                                                      Seq[(String, JsValue)] = {
    if (typeOfStudy.isEmpty) {
      if (auxSeq.isEmpty) search(params, maxDocs).map(elem => ("other", elem))
      else auxSeq
    } else {
      val tos = typeOfStudy.head
      // 'Question and answer' is not a type of study but a type of document.
      val newExpr = if (tos.equals("question_answer")) " AND (type:\"perg_resp\")"
        else " AND (type_of_study:" + "\"" + tos + "\")"
      val params2: Seq[(String,String)] = params.map {
        param => if (param._1 equals "q") ("q" -> (param._2 + newExpr))
                 else param
      }
      val numDocsAnd = auxDocsAnd.map(_.count(_._1 equals tos)).getOrElse(0)
      val maxDocs2 = maxDocs - numDocsAnd

//println(s"params2=$params2")
      val docs = if (maxDocs2 > 0) search(params2, maxDocs2) else Seq()
//println(s"[$tos] #Docs:${docs.size + numDocsAnd} and=${numDocsAnd} or=${docs.size} maxDocs2=${maxDocs2}")
println(s"[$tos] #Docs:${docs.size + numDocsAnd}")
      oSearch(typeOfStudy.tail,
              params,
              maxDocs,
              auxSeq ++ docs.map(elem => (tos, elem)),
              auxDocsAnd)
    }
  }

  private def search(params: Seq[(String, String)],
                     maxDocs: Int): Seq[JsValue] = {
    require(params != null)
    require(maxDocs > 0)

    post(params) match {
      case Right(ctt) => (Json.parse(ctt) \ "response" \ "docs").validate[JsArray] match {
        case res: JsResult[JsArray] => res.get.value.take(maxDocs)
        case _                      => Seq()
      }
      case Left(err) =>
        println(s"FAILURE=$err")
        Seq()
    }
  }

  /**
    * Sends via http/post protocol a sequence of parameters in the form (key,value)
    *
    * @param params the sequence of post parameters: key=value
    * @return Right(response) where response is the content of the post JsResult
    * if everything was ok or Left(err) where err is the error message is Some
    * error occurred
    */
  private def post(params: Seq[(String, String)]): Either[String, String] = {
    val paramSeq = params.map(elem => new BasicNameValuePair(elem._1, elem._2))
    val httpclient = HttpClients.createDefault()
    val httpPost = new HttpPost(iahxUrl)
    httpPost.addHeader("Accept-Charset", "UTF-8")
    httpPost.addHeader("Accept", "application/json")
    httpPost.setEntity(new UrlEncodedFormEntity(paramSeq.asJava, Charset.forName("utf-8")))
println(s"httpPost=$httpPost")
    val response = httpclient.execute(httpPost)
println(s"response=$response")
    try {
      val entity = response.getEntity()
      val content = EntityUtils.toString(entity)
//println(s"content=$content entity=$entity")
      response.close()
      Right(content)
    } catch {
      case ex: Throwable =>
//println("Throwable=${ex.toString}")
        response.close()
        Left(ex.toString)
    }
  }

  private def convToInfoResponse(info: Seq[SearchParameter],
                                 docs: Seq[(String, JsValue)],
                                 outType: String,
                                 callbackFunc: Option[String]): String = {
    require(info != null)
    require(docs != null)
    require(outType != null)
    require(callbackFunc != null)

    val lang = getRespLanguage(info)
    val subtitle = info
      .filter(_.isInstanceOf[MainSearchCriteria])
      .zipWithIndex
      .foldLeft[String]("") {
        case (str, (msc, idx)) =>
          val msc2 = msc.asInstanceOf[MainSearchCriteria]
          str + (if (idx == 0) "" else " OR ") +
            (msc2.displayName.getOrElse(msc2.code.getOrElse(
              msc2.originalText.getOrElse(""))))
      }
    val categories = getCategories(info, Seq())
    val id = s"urn:uuid:${java.util.UUID.randomUUID()}"
    val feed = AtomFeed(subtitle, categories, lang = lang, id = id)
    val entries = docs.foldLeft[Seq[AtomEntry]](Seq()) {
      case (seq, (docType, doc)) =>
        seq :+ AtomEntry(docType, doc, lang, categories)
    }
    val atom = Atom(feed, entries)
    outType.toLowerCase match {
      case "application/json" => AtomOutput.toJson(atom)
      case "application/javascript" =>
        val fname = callbackFunc.getOrElse("callback")
        s"$fname(${AtomOutput.toJson(atom)});"
      case "text/xml" => AtomOutput.toXml(atom)
      case _          => AtomOutput.toXml(atom)
    }
  }

  private def getRespLanguage(info: Seq[SearchParameter]): String = {
    require(info != null)

    info.find(_.isInstanceOf[InfoRecipient]) match {
      case Some(inf: InfoRecipient) =>
        inf.lcode.getOrElse(
          info.find(_.isInstanceOf[Performer]) match {
            case Some(per: Performer) => per.lcode.getOrElse("en")
            case _                    => "en"
          }
        )
      case _ =>
        info.find(_.isInstanceOf[Performer]) match {
          case Some(per: Performer) => per.lcode.getOrElse("en")
          case _                    => "en"
        }
    }
  }

  def getCategories(info: Seq[SearchParameter],
                    aux: Seq[Category]): Seq[Category] = {
    if (info.isEmpty) aux
    else {
      val className = info.head.getClass.getName
      val (same, dif) = info.partition(_.getClass.getName.equals(className))

      same.size match {
        case 0 => aux
        case 1 => getCategories(dif, aux ++ same.head.getCategories)
        case _ =>
          val aux2 = same.zipWithIndex.foldLeft[Seq[Category]] (aux) {
            case (seq, (sparam, idx)) => sparam.getCategories.foldLeft[Seq[Category]] (seq) {
              case (seq2, cat) => seq2 :+ Category(cat.scheme + idx, cat.term)
            }
          }
          getCategories(dif, aux2)
      }
    }
  }

  /**
    * Converts a sequence of search parameters into a url defined by the protocol
    *
    * @param info sequence of SearchParameter objects
    * @param infoUrl BVS-Infobutton host url
    * @return Some(str) where str is the output url if everything was ok or
    *         None if some error occurred
    */
  def paramsToUrl(info: Seq[SearchParameter],
                  infoUrl: String): Option[String] = {
    Try (new URL(infoUrl)) match {
      case Success(u) => {
        val uStr = u.toString
        val url2 = (if (uStr.endsWith("/")) uStr.substring(0, uStr.size-1)
                   else uStr) + "?"
        val params = info.zipWithIndex.foldLeft[String]("") {
          case (out, (par, idx)) => par.getCategories.foldLeft[String](out) {
            case (out2, cat) => out2 + (if (idx == 0) "" else "&") +
                                cat.scheme + "&" + cat.term
          }
        }
        Some(url2 + URLEncoder.encode(params, "UTF-8"))
      }
      case Failure(_) => None
    }
  }

  def urlToParms(urlStr: String): Option[Map[String, String]] = {
    Try (new URL(urlStr)) match {
      case Success(u) =>
        Try {
          URLDecoder.decode(u.getQuery, "utf-8")
            .split("\\&")
            .map(_.split("="))
            .foldLeft[Map[String, String]](Map()) {
              case (map, arr) => map + ((arr(0).trim, arr(1).trim))
            }
        } match {
          case Success(u) => Some(u)
          case Failure(_) => None
        }
      case Failure(_) => None
    }
  }
}

/**
  * Object to call BVS_Infobutton.
  * usage: InfoServer <url>
  * where <url> is the url specified by the infobuuton protocol
  */
object InfoServer extends App {
  val url = if (args.isEmpty)
    "representedOrganization.id.root=[OID of the organization submitting the " +
      "request]&taskContext.c.c=PROBLISTREV&mainSearchCriteria.v.c=C18.452.394.750.149&mainSea" +
      "rchCriteria.v.cs=2.16.840.1.113883.6.177&mainSearchCriteria.v.dn=Type+2+" +
      "Diabetes+Mellitus&mainSearchCriteria.v.ot=diabetes+type+2&patientPerson" +
      ".administrativeGenderCode.c=M&age.v.v=45&age.v.u=a&informationRecipient" +
      "=PAT&performer=PROV&performer.languageCode.c=en&performer.healthCarePro" +
      "vider.c.c=163W00000X&knowledgeResponseType=application/json"
    else args(0)

  val indexDir = "web/BVSInfoButton/indexes"
  val conv = new MeshConverter(indexDir)
  val server = new InfobuttonServer(conv)

  println(s"info = ${server.getInfo(url, 10)}")
  conv.close()
}
