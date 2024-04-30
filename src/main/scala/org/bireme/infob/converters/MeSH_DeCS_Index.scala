/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.converters

import bruma.master._

import java.io.File

import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.document.{Document,Field,StoredField,StringField}
import org.apache.lucene.index.{IndexWriter,IndexWriterConfig}
import org.apache.lucene.store.FSDirectory

import org.bireme.infob.Tools._

import scala.jdk.CollectionConverters._

/**
  * Create a Lucene MeSH to DeCS index from a decs Isis master file'
  * [-mst=<path>] - path to the Isis DeCS master
  * [-index=<DeCsIndex>] - path to Lucene index to be created"
  */
object MeSH_DeCS_Index extends App {
  val DEF_DECS_INDEX = "indexes/DeCS" //"web/BVSInfoButton/indexes/DeCS"
  val DEF_DECS_MASTER = "isis/decs" //"/usr/local/bireme/tabs/decs"

  val MESH_ID_TAG = 480
  val HIERARCHICAL_CODE_TAG = 20
  val ENGLISH_DESCR_TAG = 1
  val SPANISH_DESCR_TAG = 2
  val PORTUGUESE_DESCR_TAG = 3

  val parameters = args.foldLeft[Map[String,String]](Map()) {
    case (map,par) =>
      val split = par.split(" *= *", 2)
      map + ((split(0).substring(1), split(1)))
  }
  val index = parameters.getOrElse("index", DEF_DECS_INDEX)
  val mstPath = parameters.getOrElse("mst", DEF_DECS_MASTER)

  createIndex(mstPath, index)

  def createIndex(deCSMaster: String,
                  deCSIndex: String): Unit =  {

    val mst = MasterFactory.getInstance(deCSMaster).open()
    val analyzer = new KeywordAnalyzer()
    val directory = FSDirectory.open(new File(deCSIndex).toPath)

    val config = new IndexWriterConfig(analyzer)
    config.setOpenMode(IndexWriterConfig.OpenMode.CREATE)

    val indexWriter = new IndexWriter(directory, config)

    mst.iterator().asScala.foreach(writeRecord(_, indexWriter))

    indexWriter.forceMerge(1)
    indexWriter.close()
    directory.close()
    mst.close()
  }

  private def writeRecord(rec: Record,
                          indexWriter: IndexWriter): Unit = {
    if (rec.isActive) {
      val idFld = rec.getField(MESH_ID_TAG, 1)
      val id = if (idFld == null) None else Some(idFld.getContent)
      val hCodeFld = rec.getFieldList(HIERARCHICAL_CODE_TAG)
      val hCode = Option(hCodeFld)
      val eDescrFld = rec.getField(ENGLISH_DESCR_TAG, 1)
      val eDescr = if (eDescrFld == null) None else Some(eDescrFld.getContent)
      val sDescrFld = rec.getField(SPANISH_DESCR_TAG, 1)
      val sDescr = if (sDescrFld == null) None else Some(sDescrFld.getContent)
      val pDescrFld = rec.getField(PORTUGUESE_DESCR_TAG, 1)
      val pDescr = if (pDescrFld == null) None else Some(pDescrFld.getContent)

      id.map(id2 => hCode.map(hCode2 => eDescr.map(eDescr2 => sDescr.map(
        sDescr2 => pDescr.map(pDescr2 => {
          val doc = new Document()
          doc.add(new StringField("MESH_ID", id2,  Field.Store.YES))
          hCode2.asScala.foreach { hc =>
            doc.add(new StringField("HIERARCHICAL_CODE", hc.getContent, Field.Store.YES))
          }
          doc.add(new StoredField("ENGLISH_DESCR", eDescr2))
          doc.add(new StoredField("SPANISH_DESCR", sDescr2))
          doc.add(new StoredField("PORTUGUESE_DESCR", pDescr2))
          doc.add(new StringField("ENGLISH_DESCR_NORM", uniformString(eDescr2),  Field.Store.YES))
          doc.add(new StringField("SPANISH_DESCR_NORM", uniformString(sDescr2),  Field.Store.YES))
          doc.add(new StringField("PORTUGUESE_DESCR_NORM", uniformString(pDescr2),  Field.Store.YES))
          indexWriter.addDocument(doc)
        }
      )))))
    }
  }
}
