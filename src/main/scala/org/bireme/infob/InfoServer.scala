/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

import org.bireme.infob.parameters._

import play.api.libs.json._

import scala.collection.JavaConverters._
import scala.io.Source
import scala.reflect.runtime.universe._
import scala.util.{Try, Success, Failure}

class InfobuttonServer(conv: MeshConverter,
                       iahxUrl: String =
                        "http://basalto02.bireme.br:8986/solr5/portal/select") {

  def getInfo(param: java.util.Map[String, String],
              maxDocs: Int): String = getInfo(param.asScala.toMap, maxDocs)

  def getInfo(param: Map[String, String],
              maxDocs: Int = 10): String = {
    require(param != null)
    require(maxDocs > 0)

    val (info, outType, callbackFunc) = ParameterParser.parse(param)

    getInfo(info, outType.getOrElse("application/json"), maxDocs, callbackFunc)
  }

  private def getInfo(info: Seq[SearchParameter],
                      outType: String,
                      maxDocs: Int,
                      callbackFunc: Option[String]): String = {
    require(info != null)
    require(outType != null)
    require(maxDocs > 0)
    require(callbackFunc != null)

    val expr = convToSrcExpression(info, conv, maxDocs)

    convToInfoResponse(expr, info, search(expr), outType, callbackFunc)
  }

//http://basalto02.bireme.br:8986/solr5/portal/select?q=tw:((instance:%22regional%22)%20AND%20(%20mh:(c02.081.270)))&wt=json&indent=true&start=0&rows=10&sort=da+desc
  private def convToSrcExpression(info: Seq[SearchParameter],
                                  conv: MeshConverter,
                                  maxDocs: Int): Option[String] = {
    val expr = MainSearchCriteriaSrcExpr(info, conv) match {
      case Some(msc_str) => Some(
        s"$iahxUrl?start=0&rows=$maxDocs&sort=da+desc&wt=json" +
        s"&q=tw:$msc_str%20AND%20(instance:%22regional%22)%20AND%20" +
        s"(fulltext:(%221%22))" +
        OtherSrcExpr[AdministrativeGenderCode](info, conv) +
        OtherSrcExpr[Age](info, conv) +
        OtherSrcExpr[AgeGroup](info, conv) +
        OtherSrcExpr[InfoRecipient](info, conv) +
        OtherSrcExpr[Performer](info, conv)
      )
      case None => None
    }
    expr match {
      case Some(e) => println(s"expressao=$e")
      case None => println ("nenhuma expressao")
    }
    expr
  }

  private def MainSearchCriteriaSrcExpr(info: Seq[SearchParameter],
                                        conv: MeshConverter): Option[String] =
    info.filter(_.isInstanceOf[MainSearchCriteria]).
      map(_.toSrcExpression(conv)).flatten.sorted.mkString("%20OR%20") match {
        case "" => None
        case str => Some(s"($str)")
      }

  private def OtherSrcExpr[T: TypeTag](info: Seq[SearchParameter],
                                       conv: MeshConverter): String = {
    val typeName = typeOf[T].typeSymbol.asClass.fullName

    info.find(sp => sp.getClass.getName.equals(typeName)).
                                            flatMap(_.toSrcExpression(conv)) match {
      case Some(expr) => expr
      case None => ""
    }
  }

  private def search(expression: Option[String]): Seq[JsValue] = {
    require(expression != null)

    expression match {
      case Some(url) => {
println(s"url=$url")
        Try(Source.fromURL(url, "utf-8").getLines().mkString("\n")) match {
          case Success(ctt) =>
            (Json.parse(ctt) \ "response" \ "docs").validate[JsArray] match {
              case res: JsResult[JsArray] => res.get.value
              case _ => Seq()
            }
          case Failure(_) => Seq()
        }
      }
      case None => Seq()
    }
  }

  private def convToInfoResponse(expr: Option[String],
                                 info: Seq[SearchParameter],
                                 docs: Seq[JsValue],
                                 outType: String,
                                 callbackFunc: Option[String]): String = {
    require(expr != null)
    require(info != null)
    require(docs != null)
    require(outType != null)
    require(callbackFunc != null)

    val lang = getRespLanguage(info)
    val subtitle = info.filter(_.isInstanceOf[MainSearchCriteria]).zipWithIndex.
      foldLeft[String]("") {
        case (str, (msc, idx)) =>
          val msc2 = msc.asInstanceOf[MainSearchCriteria]
          str + (if (idx == 0) "" else " OR ") +
          (if (msc2.displayName.isEmpty) msc2.code.get else msc2.displayName.get)
      }
    val categories:Seq[Category] = info.flatMap(_.getCategories)
    val id = s"urn:bvs:${expr.getOrElse("").hashCode}"
    val feed = AtomFeed(subtitle, categories, lang=lang, id=id)
    val entries = docs.foldLeft[Seq[AtomEntry]](Seq()) {
      case (seq, doc) => seq :+ AtomEntry(doc, lang, categories)
    }
    val atom = Atom(feed,entries)
    outType.toLowerCase match {
      case "application/json" => AtomOutput.toJson(atom)
      case "application/javascript" =>
        val fname = callbackFunc.getOrElse("callback")
        s"$fname(${AtomOutput.toJson(atom)});"
      case "text/xml" => AtomOutput.toXml(atom)
      case _ => AtomOutput.toXml(atom)
    }
  }

  private def getRespLanguage(info: Seq[SearchParameter]): String = {
    require(info != null)

    info.find(_.isInstanceOf[InfoRecipient]) match {
      case Some(inf:InfoRecipient) => inf.lcode match {
        case Some(lang) => lang
        case None => info.find(_.isInstanceOf[Performer]) match {
          case Some(per:Performer) => per.lcode match {
            case Some(lang) => lang
            case None => "en"
          }
        }
      }
      case Some(per:Performer) => per.lcode match {
        case Some(lang) => lang
        case None => "en"
      }
      case _ => "en"
    }
  }
}

object InfoServer extends App {

  val url =
"representedOrganization.id.root=[OID of the organization submitting the" +
"request]&taskContext.c.c=PROBLISTREV&mainSearchCriteria.v.c=C18.452.394.750.149&mainSea" +
"rchCriteria.v.cs=2.16.840.1.113883.6.177&mainSearchCriteria.v.dn=Type+2+" +
"Diabetes+Mellitus&mainSearchCriteria.v.ot=diabetes+type+2&patientPerson" +
".administrativeGenderCode.c=M&age.v.v=45&age.v.u=a&informationRecipient" +
"=PAT&performer=PROV&performer.languageCode.c=en&performer.healthCarePro" +
"vider.c.c=163W00000X&knowledgeResponseType=application/javascript"

  val map = url.split("\\&").map(_.trim.split("=")).
    foldLeft[Map[String,String]](Map()) {
      case (map,arr) => map + ((arr(0).trim,arr(1).trim))
    }
  val conv = new MeshConverter()
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

  val msc1 = new MainSearchCriteria(code=Some(args(0)))
  val msc2 = new MainSearchCriteria(code=Some(args(1)))
  val conv = new MeshConverter()

  val teste = new InfoRecipient(role=Some("PAT"), langCode=Some("pt"))

  val server = new InfobuttonServer(conv)

  val info = server.getInfo(List(msc1, msc2, teste), "xml")

  println(s"info = $info")
}
*/
