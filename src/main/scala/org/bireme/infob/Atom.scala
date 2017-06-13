package org.bireme.infob

case class Category(schema: String,
                    term: String)

case class Link(href: String,
                hreflang: String,
                title: String,
                rel: String = "alternate",
                _type: String = "html")

case class Atom(feed: Feed,
                entry: Seq[Entry])

case class Feed(id: String,
                lang: String,
                subtitle: String,
                update: String,
                categories: Seq[Category],
                entries: Seq[Entry])

  case class Entry(id: String,
                   lang: String,
                   updated: String,
                   summary: String,
                   source: String,
                   categories: Seq[Category],
                   links: Seq[Link]
                 )
