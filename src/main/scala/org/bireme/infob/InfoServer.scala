package org.bireme.infob

import play.api.libs.json._

import scala.io.Source
import scala.util.{Try, Success, Failure}

trait SearchParameter {
  def tryToString(): Option[String]
}

class MainSearchCriteria(code: Option[String] = None,
                         codeSystem: Option[String] = None,
                         displayName: Option[String] = None,
                         originalText: Option[String] = None)
                                                       extends SearchParameter {
  override def tryToString(): Option[String] = {
    None
  }
}

abstract class InfoRecipient(langCodeSystem: Option[String] = None,
                             langCode: Option[String] = None,
                             langDisplayName: Option[String] = None)
                                                       extends SearchParameter {
  val lcode = langCodeSystem match {
    case Some("ISO 639-1") =>
      langCode match {
        case Some("en") => "en"
        case Some("es") => "es"
        case Some("pt") => "pt"
        case Some("fr") => "fr"
        case Some("zh") => "zh"
        case Some("de") => "de"
        case Some("ru") => "ru"
        case Some("jv") => "jv"
        case Some("nl") => "nl"
        case Some("ar") => "ar"
        case Some("pl") => "pl"
        case Some("da") => "da"
        case Some("it") => "it"
        case Some("no") => "no"
        case _ => new IllegalArgumentException()
      }
    case _ => langDisplayName match {
      case Some(code) => code.toLowerCase() match {
        case "english" => "en"
        case "spanish" => "es"
        case "portuguese" => "pt"
        case "french" => "fr"
        case "chinese" => "zh"
        case "german" => "de"
        case "russian" => "ru"
        case "japanese" => "jv"
        case "dutch" => "nl"
        case "arabic" => "ar"
        case "polish" => "pl"
        case "danish" => "da"
        case "italian" => "it"
        case "norwegian" => "no"
        case _ => new IllegalArgumentException()
      }
      case None => new IllegalArgumentException()
    }
  }
}

class InfoRecipientProv(code: Option[String] = None,
                        codeSystem: Option[String] = None,
                        displayName: Option[String] = None,
                        langCodeSystem: Option[String] = None,
                        langCode: Option[String] = None,
                        langDisplayName: Option[String] = None)
                                 extends InfoRecipient(langCodeSystem, langCode,
                                                              langDisplayName) {
  override def tryToString(): Option[String] = {
    None
  }
}

class InfoRecipientPat(langCodeSystem: Option[String] = None,
                       langCode: Option[String] = None,
                       langDisplayName: Option[String] = None)
                                 extends InfoRecipient(langCodeSystem, langCode,
                                                              langDisplayName) {
  override def tryToString(): Option[String] = {
    None
  }
}

class InfoRecipientPayor(langCodeSystem: Option[String] = None,
                         langCode: Option[String] = None,
                         langDisplayName: Option[String] = None)
                                 extends InfoRecipient(langCodeSystem, langCode,
                                                              langDisplayName) {
  override def tryToString(): Option[String] = {
    None
  }
}

class InfobuttonServer(conv: MeshConverter,
                       iahxUrl: String =
                        "http://basalto02.bireme.br:8986/solr5/portal/select") {

  def getInfo(info: List[SearchParameter],
              outType: String,
              maxDocs: Int): String = {
    convToInfoResponse(search(convToSrcExpression(info, conv, maxDocs)), outType)
  }

  private def convToSrcExpression(info: List[SearchParameter],
                                  conv: MeshConverter,
                                  maxDocs: Int= 10): Option[String] = {

    //http://basalto02.bireme.br:8986/solr5/portal/select?q=tw:((instance:%22regional%22)%20AND%20(%20mh:(c02.081.270)))&wt=json&indent=true&start=0&rows=10&sort=da+desc
    info.foldLeft[String]("") {
      case (str,param) => param.tryToString match {
        case Some(paramStr) =>  str + paramStr
        case None => str
      }
    } match {
      case "" => None
      case str => Some(s"$iahxUrl?q=start=0&rows=$maxDocs&sort=da+desc$str")
    }
  }

  private def search(expression: Option[String]): Seq[JsValue] = {
    expression match {
      case Some(expr) => {
        val url = s"$iahxUrl/?$expr"

        Try(Source.fromURL(url, "utf-8").getLines().mkString("\n")) match {
          case Success(ctt) =>
            (Json.parse(ctt) \ "response" \ "result" \ "doc").validate[JsArray] match {
              case res: JsResult[JsArray] => res.get.value
              case _ => Seq()
            }
          case Failure(_) => Seq()
        }
      }
      case None => Seq()
    }
  }

  private def convToInfoResponse(docs: Seq[JsValue],
                                 outType: String): String = {
   ""
  }
}
