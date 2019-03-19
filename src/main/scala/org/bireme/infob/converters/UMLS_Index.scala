/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.converters

import bruma.master._
import java.io.File

import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.document.{Document, Field, StringField}
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.store.FSDirectory
import org.bireme.infob.Tools

import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet
import scala.io.{BufferedSource, Source}

/**
  * Create a Lucene UMLS index from a UMLS Isis master file'
  * [-mst=<path>] - path to the Isis UMLS master
  * [-index=<UMLSIndex>] - path to Lucene index to be created"
  * [-snomed=<path>] - path to Snomed-CT text file"
  */
object UMLS_Index extends App {
  case class EntryTerm(umlsConceptCode: String,
                       thesaurus: String,
                       termCode: String,
                       termLabel: String,
                       termType: String)

  val DEF_UMLS_INDEX = "web/BVSInfoButton/indexes/UMLS"
  val DEF_UMLS_YEAR = 2018
  val DEF_UMLS_MASTER =  "decsnamecuisty" //s"/bases/umls/${DEF_UMLS_YEAR}AA/decsnamecuisty"
  val DEF_SNOMED_CT_FILE = "Snomed-CT/sct2_Description_Full-en_INT_20180731.txt"

  val thesauri = HashSet("ICD9CM", "ICD10CM", "ICD10", "SNOMEDCT_US", "RXNORM",
    "NDC", "LOINC")

  val conversion = Map("ICD9CM" -> "ICD9-CM", "ICD10CM" -> "ICD10-CM",
    "ICD10" -> "ICD10", "SNOMEDCT_US" -> "SNOMED-CT", "RXNORM" -> "RXNORM",
    "NDC" -> "NDC", "LOINC" -> "LOINC")

  val TAG = 501
  val UMLS_CONCEPT_CODE_SUBFLD = '1'
  val THESAURUS_SUBFLD = 'd'
  val TERM_CODE_SUBFLD = 'c'
  val TERM_LABEL_SUBFLD = 'q'
  val TERM_TYPE_SUBFLD = 't'

  val parameters = args.length match {
    case 0 => Map[String, String]()
    case _ => args.foldLeft[Map[String, String]](Map()) {
      case (map, par) =>
        val split = par.split(" *= *", 2)
        map + ((split(0).substring(1), split(1)))
    }
  }
  val index = parameters.getOrElse("index", DEF_UMLS_INDEX)
  val mstPath = parameters.getOrElse("mst", DEF_UMLS_MASTER)
  val snomedPath = parameters.getOrElse("snomed", DEF_SNOMED_CT_FILE)

  createIndex(mstPath, index, snomedPath)

  def createIndex(umlsMaster: String,
                  umlsIndex: String,
                  snomedCT: String): Unit = {

    val mst = MasterFactory.getInstance(umlsMaster).open()
    val analyzer = new KeywordAnalyzer()
    val directory = FSDirectory.open(new File(umlsIndex).toPath)

    val config = new IndexWriterConfig(analyzer)
    config.setOpenMode(IndexWriterConfig.OpenMode.CREATE)

    val indexWriter = new IndexWriter(directory, config)

    writeSnomedCT(snomedCT, indexWriter)

    mst.iterator().asScala.foreach(writeRecord(_, indexWriter))

    indexWriter.forceMerge(1)
    indexWriter.close()
    directory.close()
    mst.close()
  }

  private def writeSnomedCT(file: String,
                            indexWriter: IndexWriter): Unit = {
    val source: BufferedSource = Source.fromFile(file, "utf-8")

    source.getLines().foreach {
      line =>
        val linet: String = line.trim
        val split: Array[String] = linet.split("\t")
        if (split.length == 9) {
          val doc: Document = new Document()
          doc.add(new StringField("thesaurus", "SNOMED-CT", Field.Store.YES))
          doc.add(new StringField("termCode", split(4), Field.Store.YES))
          doc.add(new StringField("termLabel", split(7), Field.Store.YES))
          doc.add(new StringField("termLabelNorm",
            Tools.uniformString(split(7)), Field.Store.YES))
          indexWriter.addDocument(doc)
        }
    }
    source.close()
  }

  private def writeRecord(rec: Record,
                          indexWriter: IndexWriter): Unit = {
    parseRecord(rec).foreach {
      _.groupBy[String](_.umlsConceptCode).foreach {
        case (code, etSeq) =>
          val seqs = etSeq.groupBy[String](et =>
            if (et.thesaurus.startsWith("MSH")) "MESH" else "NO_MESH")
          val meshCode = seqs.get("MESH").map(_.head.termCode)

          seqs.get("NO_MESH").foreach { noMshSeq =>
            thesauri.foreach { thes =>
              val tSeq = noMshSeq.filter { et => et.thesaurus.equals(thes) }
              writeDocuments(tSeq, code, meshCode, indexWriter)
            }
          }
      }
    }
  }

  private def writeDocuments(etSeq: Seq[EntryTerm],
                             umlsCode: String,
                             meshTermCode: Option[String],
                             indexWriter: IndexWriter): Unit = {
    if (etSeq.nonEmpty) {
      val head: EntryTerm = etSeq.head
      val preferedTerm: Option[EntryTerm] = etSeq.find { et => et.termType.equals("PF")}
      val doc: Document = new Document()

      val thesaurus: String = conversion(head.thesaurus)
      doc.add(new StringField("umlsCode", umlsCode, Field.Store.YES))
      doc.add(new StringField("thesaurus", thesaurus, Field.Store.YES))
      doc.add(new StringField("termCode", head.termCode, Field.Store.YES))
      meshTermCode.foreach { mtc =>
        doc.add(new StringField("meshCode", mtc, Field.Store.YES))
      }
      preferedTerm match {
        case Some(et) =>
          doc.add(new StringField("termLabel", et.termLabel,
            Field.Store.YES))
          doc.add(new StringField("termLabelNorm",
            Tools.uniformString(et.termLabel), Field.Store.YES))
        case None =>
          doc.add(new StringField("termLabel", head.termLabel,
            Field.Store.YES))
          doc.add(new StringField("termLabelNorm",
            Tools.uniformString(head.termLabel), Field.Store.YES))
      }
      indexWriter.addDocument(doc)
      ()
    }
  }

  private def parseRecord(rec: Record): Option[Seq[EntryTerm]] = {
    if (rec.isActive) {
      val et = rec.iterator.asScala
        .filter(_.getId == TAG)
        .foldLeft[Seq[EntryTerm]](Seq()) {
          case (seq, fld) =>
            seq :+ EntryTerm(
              fld.getSubfield(UMLS_CONCEPT_CODE_SUBFLD, 1).getContent,
              fld.getSubfield(THESAURUS_SUBFLD, 1).getContent,
              fld.getSubfield(TERM_CODE_SUBFLD, 1).getContent,
              fld.getSubfield(TERM_LABEL_SUBFLD, 1).getContent,
              fld.getSubfield(TERM_TYPE_SUBFLD, 1).getContent
            )
        }
      Some(et)
    } else {
      None
    }
  }
}
