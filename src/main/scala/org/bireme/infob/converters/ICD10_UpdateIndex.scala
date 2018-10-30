/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.converters

import java.io.File

import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.document.{Document, Field, StringField}
import org.apache.lucene.index.{DirectoryReader, IndexWriter, IndexWriterConfig, Term}
import org.apache.lucene.search.{IndexSearcher, TermQuery}
import org.apache.lucene.store.FSDirectory

import org.bireme.infob.Tools

import scala.io.Source

/**
  * Update a Lucene UMLS index from a text file with the following format:'
  *     <ICD10_CODE>;<ICD10_DESCRIPTION>
  * The update will not have umls code but will help to do brute force search
  * [-file=<path>] - path to the input file
  * [-index=<UMLSIndex>] - path to Lucene index to be updated"
  */
object ICD10_UpdateIndex extends App {
  val DEF_LUCENE_INDEX = "web/BVSInfoButton/indexes/UMLS"
  val DEF_FILE = "cid10/ver2010/all.txt"

  val parameters = args.foldLeft[Map[String, String]](Map()) {
    case (map, par) =>
      val split = par.split(" *= *", 2)
      map + ((split(0).substring(1), split(1)))
  }
  val index = parameters.getOrElse("index", DEF_LUCENE_INDEX)
  val url = parameters.getOrElse("url", DEF_FILE)

  updateIndex(index, url)

  private def updateIndex(luceneIndex: String,
                          inputFile: String): Unit = {
    val analyzer = new KeywordAnalyzer()
    val directory = FSDirectory.open(new File(luceneIndex).toPath)
    val source = Source.fromFile(inputFile, "utf-8")
    val lines = source.getLines()

    val config = new IndexWriterConfig(analyzer)
    config.setOpenMode(IndexWriterConfig.OpenMode.APPEND)
    val indexWriter = new IndexWriter(directory, config)

    insertDefinitions(lines, indexWriter, None)
    indexWriter.close()

    val config2 = new IndexWriterConfig(analyzer)
    config2.setOpenMode(IndexWriterConfig.OpenMode.APPEND)
    val indexWriter2 = new IndexWriter(directory, config2)
    indexWriter2.forceMerge(1)
    indexWriter2.close()
    
    directory.close()
    source.close()
  }

  private def insertDefinitions(lines: Iterator[String],
                                writer: IndexWriter,
                                searcher: Option[IndexSearcher]): Unit = {
    if (lines.hasNext) {
      val line = lines.next.trim
      val split = line.split(";", 2)
      if (split.size != 2) insertDefinitions(lines, writer, searcher)
      val code = split(0)
      val termLabel = split(1)
      val searcher2 = searcher.getOrElse(new IndexSearcher(DirectoryReader.open(writer)))
      val topDocs = searcher2.search(new TermQuery(new Term("termCode", code)), 1)
      if (topDocs.totalHits == 0) {
        val doc = new Document()

        doc.add(new StringField("thesaurus", "ICD10", Field.Store.YES))
        doc.add(new StringField("termCode", code, Field.Store.YES))
        doc.add(new StringField("termLabel", termLabel, Field.Store.YES))
        doc.add(new StringField("termLabelNorm",
          Tools.uniformString(termLabel), Field.Store.YES))
        writer.addDocument(doc)
        searcher2.getIndexReader.close()
        insertDefinitions(lines, writer, None)
      } else {
        insertDefinitions(lines, writer, searcher)
      }
    } else if (searcher.isDefined) searcher.get.getIndexReader.close()
  }
}
