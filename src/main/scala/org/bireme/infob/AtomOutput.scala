/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

import org.dom4j.{DocumentHelper, Document, Element}
import play.api.libs.json._

object AtomOutput {
  def toXml(atom: Atom): String = {
    require(atom != null)

    val document = DocumentHelper.createDocument()
    document.setXMLEncoding("UTF-8")

    val feedElement = feed2Xml(atom.feed, document)

    atom.entry.foreach(entry => entry2Xml(entry, feedElement))

    document.asXML()
  }

  def toJson(atom: Atom): String = {
    require(atom != null)

    val feed = feed2Json(atom.feed)
    val atomEntry = atom.entry

    if (atomEntry.isEmpty)
      Json.prettyPrint(JsObject(List("feed" -> JsObject(feed))))
    else {
      val entries = Seq("entry" -> JsArray(atomEntry.map(entry2Json(_))))

      Json.prettyPrint(JsObject(List("feed" -> JsObject(feed ++ entries))))
      //Json.stringify(JsObject(List("feed" -> JsObject(feed ++ entries))))
    }
  }

  private def feed2Xml(feed: AtomFeed, doc: Document): Element = {
    require(feed != null)
    require(doc != null)

    val root = doc.addElement("feed")
    //root.addAttribute("xml:xmlns", "xxx")
    //root.addAttribute("yxmlns", "yyy")
    root
      .addAttribute("xmlns:", "http://www.w3.org/2005/Atom")
      .addAttribute("xml:base", feed.link)
      .addAttribute("xml:lang", feed.lang)

    val rights = root.addElement("rights")
    rights.addElement("name").addText(feed.rights._1)
    rights.addElement("uri").addText(feed.rights._2)

    root.addElement("icon").addText(feed.icon)

    root.addElement("title").addAttribute("type", "text").addText(feed.title)

    root
      .addElement("subtitle")
      .addAttribute("type", "text")
      .addText(feed.subtitle)

    val author = root.addElement("author")
    author.addElement("name").addText(feed.author._1)
    author.addElement("uri").addText(feed.author._2)

    root.addElement("updated").addText(feed.updated)

    feed.categories
      .filter(cat => (!cat.scheme.isEmpty && !cat.term.isEmpty))
      .foreach(
        cat =>
          root
            .addElement("category")
            .addAttribute("scheme", cat.scheme)
            .addAttribute("term", cat.term))

    root.addElement("id").addText(feed.id)
    root
  }

  private def entry2Xml(entry: AtomEntry, feedElem: Element): Unit = {
    require(entry != null)
    require(feedElem != null)

    val entryElem =
      feedElem.addElement("entry").addAttribute("xml:lang", entry.lang)

    entry.title.map(title => entryElem.addElement("title").addText(title))

    entry.link.map(
      link =>
        entryElem
          .addElement("link")
          .addAttribute("href", link)
          .addAttribute("rel", "via")
          .addAttribute("type", "html")
          .addAttribute("hreflang", entry.lang)
          .addAttribute("title", entry.title.getOrElse("")))

    entry.id.map(id => entryElem.addElement("id").addText(id))

    entry.updated.map(updated =>
      entryElem.addElement("updated").addText(updated))

    entry.summary.map(
      summary =>
        entryElem
          .addElement("summary")
          .addAttribute("type", "text")
          .addText(summary))

    entry.categories
      .filter(cat => (!cat.scheme.isEmpty && !cat.term.isEmpty))
      .foreach(
        cat =>
          entryElem
            .addElement("category")
            .addAttribute("scheme", cat.scheme)
            .addAttribute("term", cat.term))

    entry.docType.map(
      docType =>
        entryElem
          .addElement("category")
          .addAttribute("scheme", "documentType")
          .addAttribute("term", docType))

    entry.author.map(_.foreach(author =>
      entryElem.addElement("author").addText(author)))
    entry.source.map(source => entryElem.addElement("source").addText(source))
  }

  private def feed2Json(feed: AtomFeed): Seq[(String, JsValue)] = {
    require(feed != null)

    val headerCategory = JsArray(
      feed.categories
        .filter(cat => (!cat.scheme.isEmpty && !cat.term.isEmpty))
        .map(cat =>
          JsObject(List("scheme" -> JsString(cat.scheme),
                        "term" -> JsString(cat.term))))
    )
    Seq(
      "xsi" -> JsString("http://www.w3.org/2001/XMLSchema-instance"),
      "rights" -> JsObject(
        List("_value" -> JsString(feed.rights._1),
             "uri" -> JsString(feed.rights._2))),
      "icon" -> JsString(feed.icon),
      "base" -> JsString(feed.link),
      "lang" -> JsString(feed.lang),
      "title" -> JsObject(
        List("_value" -> JsString(feed.title), "type" -> JsString("text"))),
      "subtitle" -> JsObject(
        List("_value" -> JsString(feed.subtitle), "type" -> JsString("text"))),
      "author" -> JsObject(
        List("_name" -> JsObject(List("_value" -> JsString(feed.author._1))),
             "uri" -> JsObject(List("_value" -> JsString(feed.author._2))))),
      "updated" -> JsObject(List("_value" -> JsString(feed.updated))),
      "category" -> headerCategory,
      "id" -> JsObject(List("_value" -> JsString(feed.id)))
    )
  }

  private def entry2Json(entry: AtomEntry): JsObject = {
    require(entry != null)

    val docType = entry.docType match {
      case Some(t) =>
        Seq(
          JsObject(
            List("scheme" -> JsString("documentType"), "type" -> JsString(t))))
      case _ => Seq()
    }
    val category = Some(
      JsArray(
        entry.categories
          .filter(cat => (!cat.scheme.isEmpty && !cat.term.isEmpty))
          .map(cat =>
            JsObject(List("scheme" -> JsString(cat.scheme),
                          "term" -> JsString(cat.term)))) ++ docType
      ))
    val seq = Seq[(String, Option[JsValue])](
      "summary" -> entry.summary.map(sum =>
        JsObject(List("_value" -> JsString(sum), "type" -> JsString("text")))),
      "id" -> entry.id.map(id => JsObject(List("_value" -> JsString(id)))),
      "category" -> category,
      "title" -> entry.title.map(tit =>
        JsObject(List("_value" -> JsString(tit), "type" -> JsString("text")))),
      "updated" -> entry.updated.map(upd =>
        JsObject(List("_value" -> JsString(upd)))),
      "link" -> entry.link.map(
        lk =>
          JsObject(
            List("hreflang" -> JsString(entry.lang),
                 "title" -> JsString(entry.title.getOrElse("")),
                 "rel" -> JsString("via"),
                 "type" -> JsString("html"),
                 "href" -> (JsString(lk))))),
      "lang" -> Some(JsString(entry.lang)),
      "author" -> entry.author.map(set => JsArray(set.map(au => JsString(au)))),
      "source" -> entry.source.map(src =>
        JsObject(List("_value" -> JsString(src))))
    )

    JsObject(seq.filterNot(_._2.isEmpty).map { case (k, v) => (k, v.get) })
  }
}
