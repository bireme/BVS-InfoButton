/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

import org.bireme.infob.parameters._

import ch.qos.logback.classic.{Level, Logger => xLogger}
import com.typesafe.scalalogging.Logger
import java.net.{URL, URLDecoder, URLEncoder}
import org.slf4j.LoggerFactory
import play.api.libs.json._

import scala.collection.JavaConverters._
import scala.io.Source
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
println(s">> getInfo - param=$param  info=$info")
    val outType = oType.getOrElse("application/json")
    val exprAND = convToSrcExpression(info, conv, maxDocs, false)
    val exprOR = convToSrcExpression(info, conv, maxDocs, true)
println("Antes do orderedSearch(exprAND, maxDocs)")
    val docsAND = orderedSearch(exprAND, maxDocs)
    val useORExpression = !exprAND.equals(exprOR)  // There are more than one MainSearchCriteria
    val remaining =  if (useORExpression) 0 else maxDocs - docsAND.size
println("Antes do orderedSearch(exprOR, maxDocs)")
    val docsOR = if (remaining > 0) orderedSearch(exprOR, remaining) else Seq()
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
                                  useOR: Boolean): Option[String] = {
    // Processing MainSearchCriteria
    val (s1,s2) = partition[MainSearchCriteria](info)

    srcParam2SrcExpr(s1, conv, useOR) map {
      msc_str =>
        // Processing LocationOfInterest
        val (s3,s4) = partition[LocationOfInterest](s2)
        val loi = srcParam2SrcExpr(s3, conv, true) match {
          case Some(src) => s"%20AND%20$src"
          case None => ""
        }
        println(s"*=>${info},$s1,$s3,$s4")

        //Processing other parameters
        s4.foldLeft[String](
          s"$iahxUrl?source=bvs_infobutton&start=0&rows=$maxDocs&sort=da+desc&wt=json" +
          s"&q=$msc_str%20AND%20(instance:%22regional%22)%20AND%20" +
          s"(fulltext:(%221%22))$loi") {
            case (str, sparam) =>
              str +
                (sparam.toSrcExpression(conv, info) match {
                  case Some(str2) => s"%20AND%20$str2"
                  case None       => ""
                })
          }
    }
  }

  private def partition[T <: SearchParameter](info: Seq[SearchParameter])
                                             (implicit ev: ClassTag[T]): // https://stackoverflow.com/questions/29886246/scala-filter-by-type
                                              (Seq[T], Seq[SearchParameter]) = {
    info.foldLeft[(Seq[T], Seq[SearchParameter])] ((Seq(), Seq())) {
      case (seq, t) => t match {
        case t: T => (seq._1 :+ t, seq._2)
        case _    => (seq._1, seq._2 :+ t)
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
      .mkString(s"%20$connector%20") match {
        case ""  => None
        case str => Some(s"($str)")
      }
  }

  private def orderedSearch(expression: Option[String],
                            maxDocs: Int): Seq[JsValue] = {

    def oSearch(typeOfStudy: Seq[String],
                expr: String,
                maxDocs: Int,
                auxSeq: Seq[JsValue]): Seq[JsValue] = {
      if (typeOfStudy.isEmpty) auxSeq
      else
        (maxDocs - auxSeq.size) match {
          case 0 => auxSeq
          case num =>
            val newExpr =
              s"$expr%20AND%20(type_of_study:(%22${typeOfStudy.head}%22))"
            oSearch(typeOfStudy.tail,
                    expr,
                    maxDocs,
                    auxSeq ++ search(newExpr, num))
        }
    }

    require(expression != null)
    require(maxDocs > 0)

    // overview is present to grant that at least some document will be returned
    val typeOfStudy = Seq("guideline",
                          "systematic_reviews",
                          "clinical_trials",
                          "cohort",
                          "case_control",
                          "case_reports",
                          "overview")
    expression match {
      case Some(url) => oSearch(typeOfStudy, url, maxDocs, Seq())
      case None      => Seq()
    }
  }

  private def search(expression: String, maxDocs: Int): Seq[JsValue] = {
    require(expression != null)
    require(maxDocs > 0)
//println(s"Pesquisando ... [$expression]")
    Try(Source.fromURL(expression, "utf-8").getLines().mkString("\n")) match {
      case Success(ctt) =>
//println(s"ctt=$ctt")
        (Json.parse(ctt) \ "response" \ "docs").validate[JsArray] match {
          case res: JsResult[JsArray] => res.get.value.take(maxDocs)
          case _                      => Seq()
        }
      case Failure(_) =>
        /*println(s"FAILURE=$x");*/
        Seq()
    }
  }

  private def convToInfoResponse(info: Seq[SearchParameter],
                                 docs: Seq[JsValue],
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
            (msc2.displayName.getOrElse(msc2.code.getOrElse("")))
      }
    val categories: Seq[Category] = info.flatMap(_.getCategories)
    val id = s"urn:uuid:${java.util.UUID.randomUUID()}"
    val feed = AtomFeed(subtitle, categories, lang = lang, id = id)
    val entries = docs.foldLeft[Seq[AtomEntry]](Seq()) {
      case (seq, doc) => seq :+ AtomEntry(doc, lang, categories)
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

object InfoServer extends App {
  val indexDir = "web/BVSInfoButton/indexes"
  val url =
    "representedOrganization.id.root=[OID of the organization submitting the " +
      "request]&taskContext.c.c=PROBLISTREV&mainSearchCriteria.v.c=C18.452.394.750.149&mainSea" +
      "rchCriteria.v.cs=2.16.840.1.113883.6.177&mainSearchCriteria.v.dn=Type+2+" +
      "Diabetes+Mellitus&mainSearchCriteria.v.ot=diabetes+type+2&patientPerson" +
      ".administrativeGenderCode.c=M&age.v.v=45&age.v.u=a&informationRecipient" +
      "=PAT&performer=PROV&performer.languageCode.c=en&performer.healthCarePro" +
      "vider.c.c=163W00000X&knowledgeResponseType=application/json"

  val map = url
    .split("\\&")
    .map(_.trim.split("="))
    .foldLeft[Map[String, String]](Map()) {
      case (map, arr) => map + ((arr(0).trim, arr(1).trim))
    }
  val conv = new MeshConverter(indexDir)
  val server = new InfobuttonServer(conv)

  val info = server.getInfo(map)

  println(s"info = $info")
}

/*
object InfoServer extends App {
  private def usage(): Unit = {
    Console.err.println("usage: InfoServer <code> <code>")
    System.exit(1)
  }

  if (args.length != 2) usage()

  val indexDir = "web/BVSInfoButton/indexes"
  val msc1 = new MainSearchCriteria(code=Some(args(0)))
  val msc2 = new MainSearchCriteria(code=Some(args(1)))
  val conv = new MeshConverter(indexDir)

  val teste = new InfoRecipient(role=Some("PAT"), langCode=Some("pt"))

  val server = new InfobuttonServer(conv)

  val info = server.getInfo(List(msc1, msc2, teste), "xml")

  println(s"info = $info")
}
 */
