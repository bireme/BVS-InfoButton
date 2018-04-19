/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

import java.util.Date
import play.api.libs.json.JsValue

case class Category(scheme: String, term: String)

case class Atom(feed: AtomFeed, entry: Seq[AtomEntry])

case class AtomFeed(subtitle: String,
                    categories: Seq[Category],
                    id: String,
                    lang: String = "en",
                    link: String = "http://pesquisa.bvsalud.org/portal/") {
  val title = lang match {
    case "en" => "VHL - Virtual Health Library"
    case "es" => "BVS - Biblioteca Virtual en Salud"
    case "pt" => "BVS - Biblioteca Virtual em Saúde"
    case "fr" => "BVS - Bibliothèque Virtuelle de Santé"
    case _    => "VHL - Virtual Health Library"
  }
  val author = lang match {
    case "es" | "pt" | "fr" => ("BIREME/OPS/OMS", "http://www.paho.org/bireme")
    case _                  => ("BIREME/PAHO/WHO", "http://www.paho.org/bireme")
  }
  val rights =
    ("BVS-InfoButton © Pan American Health Organization, 2018.",
     "https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt")

  val prefix = "http://bvsalud.org/wp-content/themes/vhl-search-portal/images"
  val suffix = "logo_bvs.jpg"
  val icon = lang match {
    case "en" => s"$prefix/en/$suffix"
    case "es" => s"$prefix/es/$suffix"
    case "pt" => s"$prefix/pt/$suffix"
    case "fr" => s"$prefix/fr/$suffix"
    case _    => s"$prefix/en/$suffix"
  }
  val updated = FormatDate.format2(new Date())
}

case class AtomEntry(dType: String,
                     doc: JsValue,
                     lang: String,
                     categories: Seq[Category]) {
  require(doc != null)
  require(lang != null)
  require(categories != null)

  val title = (doc \ s"ti_$lang").asOpt[Seq[String]] match {
    case Some(seq) => Some(seq.head)
    case None      => (doc \ "ti").asOpt[Seq[String]].map(_.head)
  }

  val entryDate = (doc \ "entry_date").asOpt[String]
  val updated = entryDate.flatMap(FormatDate.format1(_))
  val fEntryDate = entryDate.flatMap(FormatDate.format3(_))

  val author = (doc \ "au").asOpt[Seq[String]]
  val docId = (doc \ "id").asOpt[String]

  //val link = (doc \ "ur").asOpt[Seq[String]].map(_.head)
  val portal = "http://pesquisa.bvsalud.org/portal/resource"
  val link = lang match {
    case "es" => docId.map(id => s"$portal/es/$id")
    case "pt" => docId.map(id => s"$portal/pt/$id")
    case "fr" => docId.map(id => s"$portal/fr/$id")
    case _    => docId.map(id => s"$portal/en/$id")
  }

  val summary = (doc \ s"ab_$lang").asOpt[Seq[String]] match {
    case Some(seq) => Some(seq.head)
    case None      => (doc \ "ab").asOpt[Seq[String]].map(_.head)
  }
  val sumHash = summary.getOrElse(title.getOrElse("???")).hashCode.toString
  val id = Some(
    "tag:bvsalud.org," + fEntryDate.getOrElse("1970") + ":" +
      docId.getOrElse(sumHash))
  val source = (doc \ "fo").asOpt[Seq[String]].map(x => x.head.trim)
  val entryLang = (doc \ "la").asOpt[String]
  val docType = Some(dType.trim)
  //val docType = (doc \ "type").asOpt[Seq[String]].map(x => x.head.trim)
}
