/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.converters

import java.io.File

import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.document.{Document, Field, StringField}
import org.apache.lucene.index.{DirectoryReader, IndexWriter, IndexWriterConfig, Term}
import org.apache.lucene.search.{IndexSearcher, TermQuery, TopDocs}
import org.apache.lucene.store.FSDirectory
import org.bireme.infob.Tools

import scala.annotation.tailrec
import scala.io.Source

/**
  * Update a Lucene UMLS index from a text file with the following format:'
  *     <ICD10_CODE>;<ICD10_DESCRIPTION>
  * The update will not have umls code but will help to do brute force search
  * [-files=<thesName>:<path>,<thesName>:<path>,...,<thesName>:<path>] - name and path to the descriptors input files
  * [-index=<UMLSIndex>] - path to Lucene index to be updated"
  */
object UMLS_UpdateIndex extends App {
  private val DEF_LUCENE_INDEX = "web/BVSInfoButton/indexes/UMLS"
  //val DEF_FILES = "ICD10:cid10/ver2010/all.txt,ICD10:cid10/espanhol/all.txt,ICD10:cid10/datasus/ver2008/all.txt,SNOMED-CT:Snomed-CT/all.txt"
  //val DEF_FILES = "ICD10:cid10/one.txt"
  private val DEF_FILES = "ICD10:cid10/ver2010/all.txt,ICD10:cid10/espanhol/all.txt,ICD10:cid10/datasus/ver2008/all.txt"

  private val parameters = args.foldLeft[Map[String, String]](Map()) {
    case (map, par) =>
      val split = par.split(" *= *", 2)
      map + ((split(0).substring(1), split(1)))
  }
  private val files: Set[(String, String)] = parameters.getOrElse("files", DEF_FILES).split(" *, *")
    .map { elem =>
      val split = elem.split(" *: *")
      (split(0), split(1))
    }.toSet                                        // Set((thesaurus_name,path)...)
  private val index = parameters.getOrElse("index", DEF_LUCENE_INDEX)

  updateIndex(index, files)

  private def updateIndex(luceneIndex: String,
                          inputFiles: Set[(String, String)]): Unit = {
    val analyzer = new KeywordAnalyzer()
    val directory = FSDirectory.open(new File(luceneIndex).toPath)
    val config = new IndexWriterConfig(analyzer)
    config.setOpenMode(IndexWriterConfig.OpenMode.APPEND)
    val indexWriter = new IndexWriter(directory, config)

    inputFiles.foreach {
      inputFile =>
        val source = Source.fromFile(inputFile._2, "utf-8")
        val lines = source.getLines()
        println("<<< " + inputFile._2)
        insertDefinitions(inputFile._1, lines, indexWriter, None)
        source.close()
    }
    indexWriter.close()

    val config2 = new IndexWriterConfig(analyzer)
    config2.setOpenMode(IndexWriterConfig.OpenMode.APPEND)
    val indexWriter2 = new IndexWriter(directory, config2)
    indexWriter2.forceMerge(1)
    indexWriter2.close()
    
    directory.close()

  }

  @tailrec
  private def insertDefinitions(thesaurus: String,
                                lines: Iterator[String],
                                writer: IndexWriter,
                                searcher: Option[IndexSearcher]): Unit = {
    if (lines.hasNext) {
      val line = lines.next().trim
      val split = line.split("\\|", 2)
      if (split.size == 2) {
        val code: String = split(0)
        val termLabel: String = split(1)
        val searcher2: IndexSearcher = searcher.getOrElse(new IndexSearcher(DirectoryReader.open(writer)))
        val id: String = Tools.uniformString(thesaurus + "_" + code)
        val term = new Term("id", id)
        val topDocs: TopDocs = searcher2.search(new TermQuery(term), 1)

        val doc: Document = new Document()
        doc.add(new StringField("id", id, Field.Store.YES))
        doc.add(new StringField("thesaurus", thesaurus, Field.Store.YES))
        doc.add(new StringField("termCode", code, Field.Store.YES))
        if (topDocs.totalHits.value == 0) {
          doc.add(new StringField("termLabel", termLabel, Field.Store.YES))
          doc.add(new StringField("termLabelNorm", Tools.uniformString(termLabel), Field.Store.YES))
          writer.addDocument(doc)
        } else {
          //val doc1: Document = searcher2.doc(topDocs.scoreDocs.head.doc)
          val doc1: Document = searcher2.storedFields().document(topDocs.scoreDocs.head.doc)
          val labels = getLabels(doc1) + termLabel
          labels.foreach {
            label =>
              doc.add(new StringField("termLabel", label, Field.Store.YES))
              doc.add(new StringField("termLabelNorm", Tools.uniformString(label), Field.Store.YES))
          }
          writer.updateDocument(term, doc)
        }
        searcher2.getIndexReader.close()
        insertDefinitions(thesaurus, lines, writer, None)
      } else insertDefinitions(thesaurus, lines, writer, searcher)
    } else if (searcher.isDefined) searcher.get.getIndexReader.close()
  }

  private def getLabels(doc: Document): Set[String] = {
    doc.getFields("termLabel").foldLeft(Set[String]()) {
      case (set,label) => set + label.stringValue()
    }
  }
}
