/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.converters

import bruma.master._

import java.io.File

import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.document.{Document,Field,StringField}
import org.apache.lucene.index.{IndexWriter,IndexWriterConfig}
import org.apache.lucene.store.FSDirectory

import org.bireme.infob.Tools._

import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet

/**
  * Create a Lucene UMLS index from a UMLS Isis master file'
  * [-mst=<path>] - path to the Isis UMLS master
  * [-index=<UMLSIndex>] - path to Lucene index to be created"
  */
object UMLS_Index extends App {
  case class EntryTerm(umlsConceptCode: String,
                       thesaurus: String,
                       termCode: String,
                       termLabel: String)

  val DEF_UMLS_INDEX = "web/BVSInfoButton/indexes/UMLS"
  val DEF_UMLS_YEAR = 2017
  val DEF_UMLS_MASTER = s"/bases/umls/${DEF_UMLS_YEAR}AA/decsnamecuisty"

  val TAG = 501
  val UMLS_CONCEPT_CODE_SUBFLD = '1'
  val THESAURUS_SUBFLD = 'd'
  val TERM_CODE_SUBFLD = 'c'
  val TERM_LABEL_SUBFLD = 'q'

  val parameters = args.foldLeft[Map[String,String]](Map()) {
    case (map,par) =>
      val split = par.split(" *= *", 2)
      map + ((split(0).substring(1), split(1)))
  }
  val index = parameters.getOrElse("index", DEF_UMLS_INDEX)
  val mstPath = parameters.getOrElse("mst", DEF_UMLS_MASTER)

  createIndex(mstPath, index)

  def createIndex(umlsMaster: String,
                  umlsIndex: String): Unit =  {

    val mst = MasterFactory.getInstance(umlsMaster).open()
    val analyzer = new KeywordAnalyzer()
    val directory = FSDirectory.open(new File(umlsIndex).toPath())

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
    val thesauri = HashSet("ICD9CM", "ICD10CM", "IDC10", "SNOMEDCT_US", "RXNORM")

    parseRecord(rec).map {
      _.groupBy[String](_.umlsConceptCode).foreach {
        case (code, etSeq) =>
          val mesh = etSeq.groupBy[String](
            et => if (et.thesaurus.startsWith("MSH")) "MESH" else "NO_MESH")
          mesh.get("MESH").map {
            mshSeq =>
              mshSeq.foreach {
                msh =>
                  val doc = new Document()

                  doc.add(new StringField("umlsCode", code, Field.Store.YES))
                  doc.add(new StringField("thesaurus", "MESH", Field.Store.YES))
                  doc.add(new StringField("termCode", msh.termCode, Field.Store.YES))
                  doc.add(new StringField("meshCode", msh.termCode, Field.Store.YES))
                  doc.add(new StringField("termLabel", uniformString(msh.termLabel), Field.Store.YES))
                  tseq.tail.foreach(et => doc.add(new StringField("termLabel", uniformString(et.termLabel), Field.Store.YES)))
                  indexWriter.addDocument(doc)
              }
              val meshCode = mshSeq.head.termCode
              mesh.get("NO_MESH").map {
                noMshSeq => thesauri.foreach {
                  thes =>
                    val tseq = noMshSeq.filter(_.thesaurus.equals(thes))
                    if (!tseq.isEmpty) {
                      val first = tseq.head
                      val doc = new Document()

                      doc.add(new StringField("umlsCode", code, Field.Store.YES))
                      doc.add(new StringField("thesaurus", thes, Field.Store.YES))
                      doc.add(new StringField("termCode", first.termCode, Field.Store.YES))
                      doc.add(new StringField("meshCode", meshCode, Field.Store.YES))
                      doc.add(new StringField("termLabel", uniformString(first.termLabel), Field.Store.YES))
                      tseq.tail.foreach(et => doc.add(new StringField("termLabel", uniformString(et.termLabel), Field.Store.YES)))
                      indexWriter.addDocument(doc)
                    }
                }
              }
          }
      }
    }
  }

  private def parseRecord(rec: Record): Option[Seq[EntryTerm]] = {
    if (rec.isActive()) {
      val et = rec.iterator.asScala.filter(_.getId == TAG).foldLeft[Seq[EntryTerm]](Seq()) {
        case (seq,fld) => seq :+ EntryTerm(
          fld.getSubfield(UMLS_CONCEPT_CODE_SUBFLD, 1).getContent,
          fld.getSubfield(THESAURUS_SUBFLD, 1).getContent,
          fld.getSubfield(TERM_CODE_SUBFLD, 1).getContent,
          fld.getSubfield(TERM_LABEL_SUBFLD, 1).getContent
        )
      }
      Some(et)
    } else None
  }
}
