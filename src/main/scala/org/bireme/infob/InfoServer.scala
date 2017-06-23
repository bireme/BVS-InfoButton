package org.bireme.infob

import org.bireme.infob.parameters._

import play.api.libs.json._

import scala.io.Source
import scala.reflect.runtime.universe._
import scala.util.{Try, Success, Failure}

class InfobuttonServer(conv: MeshConverter,
                       iahxUrl: String =
                        "http://basalto02.bireme.br:8986/solr5/portal/select") {

  def getInfo(info: Seq[SearchParameter],
              outType: String,
              maxDocs: Int = 10): String = {
    val expr = convToSrcExpression(info, conv, maxDocs)

    convToInfoResponse(expr, info, search(expr), outType)
  }

//http://basalto02.bireme.br:8986/solr5/portal/select?q=tw:((instance:%22regional%22)%20AND%20(%20mh:(c02.081.270)))&wt=json&indent=true&start=0&rows=10&sort=da+desc
  private def convToSrcExpression(info: Seq[SearchParameter],
                                  conv: MeshConverter,
                                  maxDocs: Int): Option[String] =
    MainSearchCriteriaSrcExpr(info, conv) match {
      case Some(msc_str) => Some(
        s"$iahxUrl?start=0&rows=$maxDocs&sort=da+desc&wt=json" +
        s"&q=tw:$msc_str%20AND%20(instance:%22regional%22)%20AND%20" +
        s"(fulltext:(%221%22))" +
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
                                 outType: String): String = {
    require(expr != null)
    require(info != null)
    require(docs != null)
    require(outType != null)

    val lang = info.find(_.isInstanceOf[InfoRecipient]) match {
      case Some(inf:InfoRecipient) => inf.lcode.getOrElse("en")
      case _ => "en"
    }
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
      case "json" => AtomOutput.toJson(atom)
      case _ => AtomOutput.toXml(atom)
    }
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

  val teste = new InfoRecipientPat(langCode=Some("pt"))

  val server = new InfobuttonServer(conv)

  val info = server.getInfo(List(msc1, msc2, teste), "xml")

  println(s"info = $info")
}
