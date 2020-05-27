/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.converters

import java.io.File

import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.document.{Document, Field, StoredField, TextField}
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.store.FSDirectory

import scala.io.Source

/**
  * Create a Lucene index from a file with ICD10 code to MeSH code map
  * [-index=<luceneIndex>] - path to Lucene index to be created"
  * [-url=<url>] - url of file containing the ICD10 - Mesh map")
  */
object ICD10_MeSH_Index extends App {
  val DEF_LUCENE_INDEX = "web/BVSInfoButton/indexes/ICD10"
  val ICD10_MESH_URL = "http://svn.code.sf.net/p/diseaseontology/code/trunk/" +
    "DO_info_files/kegg_disease2xref.txt" // See http://disease-ontology.org/
  val ICD10_2010 = "cid10/ver2010/codes.txt"

  val parameters = args.foldLeft[Map[String, String]](Map()) {
    case (map, par) =>
      val split = par.split(" *= *", 2)
      map + ((split(0).substring(1), split(1)))
  }
  val index = parameters.getOrElse("index", DEF_LUCENE_INDEX)
  val url = parameters.getOrElse("url", ICD10_MESH_URL)

  createIndex(index, url, ICD10_2010)

  def createIndex(luceneIndex: String, url: String, cid10: String): Unit = {
    val analyzer = new KeywordAnalyzer()
    val directory = FSDirectory.open(new File(luceneIndex).toPath)

    val config = new IndexWriterConfig(analyzer)
    config.setOpenMode(IndexWriterConfig.OpenMode.CREATE)

    val indexWriter = new IndexWriter(directory, config)
    val source = Source.fromURL(url, "utf-8")
    val lines = source.getLines()

    index(insertDefinitions(cid10, filterByMeSHICD10(lines)), indexWriter)

    indexWriter.forceMerge(1)
    source.close()
    indexWriter.close()
    directory.close()
  }

  private def filterByMeSHICD10(
      lines: collection.Iterator[String]): Map[String, Set[String]] = {
    lines.foldLeft[Map[String, Set[String]]](Map()) {
      case (map, line) =>
        val split = line.substring(10).trim().split("\\|")

        split.find(_.startsWith("ICD-10:")) match {
          case Some(icd10) =>
            split.find(_.startsWith("MeSH:")) match {
              case Some(mesh) =>
                val meshCodes = mesh.substring(6).trim().split(" +")
                icd10
                  .substring(8)
                  .trim()
                  .split(" +")
                  .foldLeft[Map[String, Set[String]]](map) {
                    case (map2, icdCode) =>
                      val mCodes = map2.getOrElse(icdCode, Set[String]()) ++ meshCodes
                      map2 + ((icdCode, mCodes))
                  }
              case None => map
            }
          case None => map
        }
    }
  }

  private def insertDefinitions(
      cid10: String,
      codes: Map[String, Set[String]]): Map[String, (String, Set[String])] = {
    val source = Source.fromFile(cid10, "utf-8")
    val lines = source.getLines()

    val ret = lines.foldLeft[Map[String, (String, Set[String])]](Map()) {
      case (map, line) =>
        val split = line.split(";")
        val map2 = if (split.size >= 9) {
          val code = split(6).trim().toUpperCase()
          val description = split(8).trim()

          codes.get(code) match {
            case Some(mcodes) => map + (code -> (description, mcodes))
            case None         => map
          }
        } else {
          map
        }
        map2
    }
    source.close()
    ret
  }

  private def index(codes: Map[String, (String, Set[String])],
                    indexWriter: IndexWriter): Unit = {
    codes.foreach {
      case (icd10, tuple) =>
        val doc = new Document()
        doc.add(new TextField("ICD10", icd10.toUpperCase(), Field.Store.YES))
        doc.add(new StoredField("description", tuple._1))
        tuple._2.foreach { mCode =>
          doc.add(new TextField("MeSH", mCode.toUpperCase(), Field.Store.YES))
        }
        indexWriter.addDocument(doc)
    }
    indexWriter.commit()
  }
}
