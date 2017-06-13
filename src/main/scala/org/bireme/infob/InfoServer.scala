package org.bireme.infob

import play.api.libs.json._

import scala.io.Source
import scala.reflect.runtime.universe._
import scala.util.{Try, Success, Failure}

trait SearchParameter {
  def tryToString(conv: MeshConverter): Option[String]
}

class MainSearchCriteria(code: Option[String] = None,
                         codeSystem: Option[String] = None,
                         displayName: Option[String] = None,
                         originalText: Option[String] = None)
                                                       extends SearchParameter {
  override def tryToString(conv: MeshConverter): Option[String] = {
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
}

abstract class InfoRecipient(langCodeSystem: Option[String] = None,
                             langCode: Option[String] = None,
                             langDisplayName: Option[String] = None)
                                                       extends SearchParameter {
  val lcode = langCodeSystem match {
    case Some("ISO 639-1") =>
      langCode.map(_.toLowerCase) match {
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
    case _ => langDisplayName.map(_.toLowerCase) match {
      case Some("english") => "en"
      case Some("spanish") => "es"
      case Some("portuguese") => "pt"
      case Some("french") => "fr"
      case Some("chinese") => "zh"
      case Some("german") => "de"
      case Some("russian") => "ru"
      case Some("japanese") => "jv"
      case Some("dutch") => "nl"
      case Some("arabic") => "ar"
      case Some("polish") => "pl"
      case Some("danish") => "da"
      case Some("italian") => "it"
      case Some("norwegian") => "no"
      case _ => new IllegalArgumentException()
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
  override def tryToString(conv: MeshConverter): Option[String] = {
    Some("InfoRecipientProv")
  }
}

class InfoRecipientPat(langCodeSystem: Option[String] = None,
                       langCode: Option[String] = None,
                       langDisplayName: Option[String] = None)
                                 extends InfoRecipient(langCodeSystem, langCode,
                                                              langDisplayName) {
  override def tryToString(conv: MeshConverter): Option[String] = {
    Some("InfoRecipientPat")
  }
}

class InfoRecipientPayor(langCodeSystem: Option[String] = None,
                         langCode: Option[String] = None,
                         langDisplayName: Option[String] = None)
                                 extends InfoRecipient(langCodeSystem, langCode,
                                                              langDisplayName) {
  override def tryToString(conv: MeshConverter): Option[String] = {
    Some("InfoRecipientPayor")
  }
}

class InfobuttonServer(conv: MeshConverter,
                       iahxUrl: String =
                        "http://basalto02.bireme.br:8986/solr5/portal/select") {

  def getInfo(info: Seq[SearchParameter],
              outType: String,
              maxDocs: Int = 10): String = {
    convToInfoResponse(search(convToSrcExpression(info, conv, maxDocs)), outType)
  }

//http://basalto02.bireme.br:8986/solr5/portal/select?q=tw:((instance:%22regional%22)%20AND%20(%20mh:(c02.081.270)))&wt=json&indent=true&start=0&rows=10&sort=da+desc
  private def convToSrcExpression(info: Seq[SearchParameter],
                                  conv: MeshConverter,
                                  maxDocs: Int): Option[String] =
    MainSearchCriteriaSrcExpr(info, conv) match {
      case Some(msc_str) => Some(
        s"$iahxUrl?start=0&rows=$maxDocs&sort=da+desc&wt=json" +
        s"""&q=tw:((instance:%22regional%22)%20AND%20(fulltext:("1"))""" + 
        s"%20AND%20$msc_str)" +
        OtherSrcExpr[InfoRecipientProv](info, conv) +
        OtherSrcExpr[InfoRecipientPat](info, conv) +
        OtherSrcExpr[InfoRecipientPayor](info, conv)
      )
      case None => None
    }

  private def MainSearchCriteriaSrcExpr(info: Seq[SearchParameter],
                                        conv: MeshConverter): Option[String] =
    info.filter(_.isInstanceOf[MainSearchCriteria]).
      map(_.tryToString(conv)).flatten.mkString("%20OR%20") match {
        case "" => None
        case str => Some(s"($str)")
      }

  private def OtherSrcExpr[T: TypeTag](info: Seq[SearchParameter],
                                       conv: MeshConverter): String = {
    val typeName = typeOf[T].typeSymbol.asClass.fullName
    info.find(sp => sp.getClass.getName.equals(typeName)).
                                            flatMap(_.tryToString(conv)) match {
      case Some(expr) => "&" + expr
      case None => ""
    }
}

  private def search(expression: Option[String]): Seq[JsValue] = {
println(s"expression=$expression")
    expression match {
      case Some(url) => {
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

  private def convToInfoResponse(docs: Seq[JsValue],
                                 outType: String): String = {
    docs.mkString("\n")
  }
}

object InfoServer extends App {
  private def usage(): Unit = {
    Console.err.println("usage: InfoServer <code> <code>")
    System.exit(1)
  }

  if (args.length != 2) usage()

  val msc1 = new MainSearchCriteria(code=Some(args(0)))
  val msc2 = new MainSearchCriteria(code=Some(args(1)))
  val conv = new MeshConverter()

  val teste = new InfoRecipientPat(langCode=Some(args(0)))

  val server = new InfobuttonServer(conv)

  val info = server.getInfo(List(msc1, msc2, teste), "xml")

  println(s"info = $info")
}
