/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

import java.io.File

import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.index.{DirectoryReader, Term}
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search._
import org.apache.lucene.store.FSDirectory

import scala.util.{Either, Left, Right}

/**
  * Class to convert term codes from different thesaurus into correspondent
  * term codes of MESH thesaurus.
  *
  * @author Heitor Barbieri
  */
class MeshConverter(indexes: String) {
  //"ICD9CM", "ICD10CM", "IDC10", "SNOMED-CT", "RXNORM"
  private val thesauri: Map[String, String] = Map(
    "ICD9-CM" -> "ICD9-CM",
    "ICD10-CM" -> "ICD10-CM",
    "ICD10" -> "ICD10",
    "SNOMED-CT" -> "SNOMED-CT",
    "RXNORM" -> "RXNORM"
  )

  // Names of Lucene indexes to convert one thesaurus into another
  private val thes2thes: Map[String, String] = Map(
    "DeCS" -> s"$indexes/DeCS",
    "UMLS" -> s"$indexes/UMLS"
  )

  // Lucene index searchers to convert one system into another
  private val thes2thesSearchers: Map[String, IndexSearcher] = thes2thes.map {
    case (k, v) =>
      val directory: FSDirectory = FSDirectory.open(new File(v).toPath)
      require(DirectoryReader.indexExists(directory))
      val ireader: DirectoryReader = DirectoryReader.open(directory)

      (k, new IndexSearcher(ireader))
  }

  private val analyzer: KeywordAnalyzer = new KeywordAnalyzer()

  def close(): Unit = {
    thes2thesSearchers.foreach {
      case (_, src) => src.getIndexReader.close()
    }
  }

  /**
    * Convert one term code from a particular thesaurus to the code of the
    * same term but in a different thesaurus
    *
    * @param codeSystem the code of the thesaurus name
    * @param code the term code
    * @return a term code (right) if the conversion is possible
    *         or the string describing the term associated with the code (left)
    */
  def convert(codeSystem: String,
              code: String): Either[Option[Set[String]], String] = {
    require (code != null)

    val tcode: String = code.trim.toUpperCase
    if (tcode.isEmpty) {
      Left(None)
    } else {
      val mesh: Either[Option[Set[String]], String] = codeSystem.trim.toUpperCase match {
        case "2.16.840.1.113883.6.103" => getMeshCode("ICD9-CM", tcode)
        case "2.16.840.1.113883.6.90"  => getMeshCode("ICD10-CM", tcode)
        case "2.16.840.1.113883.6.3"   => getMeshCode("ICD10", tcode)
        case "2.16.840.1.113883.6.96"  => getMeshCode("SNOMED-CT", tcode)
        case "2.16.840.1.113883.6.88"  => getMeshCode("RXNORM", tcode)
        case "2.16.840.1.113883.6.177" => Right(tcode) // MeSH
        case "2.16.840.1.113883.6.69"  => getMeshCode("NDC", tcode)
        case "2.16.840.1.113883.6.1"   => getMeshCode("LOINC", tcode)
        case _                         => Left(None)
      }
      // Try converting MeSH code or term into a DeCs code or term description
      mesh match {
        case Right(mcode) =>
          mesh2DeCS(mcode) match {
            case Some(dcode) => Right(dcode)
            case None => Left(None)
          }
        case Left(x) => Left(x)
      }
    }
  }

  private def getMeshCode(codeSystem: String,
                          code: String): Either[Option[Set[String]], String] = {
    thesauri.get(codeSystem) match {
      case Some(cSystem) => thes2thesSearchers.get("UMLS") match {
        case Some(searcher) =>
          val parser: QueryParser = new QueryParser("thesaurus", analyzer)
          val query: Query = parser.parse(s"thesaurus:$cSystem AND termCode:$code")
          val topDocs = searcher.search(query, 1)
          if (topDocs.totalHits.value == 0) {
            val ucode: String = Tools.uniformString(code)
            val query2: Query = parser.parse(s"thesaurus:$cSystem AND termLabelNorm:$ucode")
            val topDocs2: TopDocs = searcher.search(query2, 10)
            if (topDocs2.totalHits.value == 0) {
              Left(None)
            } else {
              getCode(topDocs2.scoreDocs.map(_.doc), searcher)
            }
          } else {
            getCode(topDocs.scoreDocs.map(_.doc), searcher)
          }
        case None => Left(None)
      }
      case None => Left(None)
    }
  }

  /**
  * Look for the Mesh code equivalent of a thesauri code
    * @param docIds the Lucene document ids of document that have the given thesauri code
    * @param searcher searcher of UMLS index
    * @return the equivalent Mesh code or the textual term descriptions of the found document
    */
  private def getCode(docIds: Array[Int],
                      searcher: IndexSearcher): Either[Option[Set[String]], String] = {
    docIds.map(id => searcher.storedFields().document(id)).find(doc1 => doc1.get("meshCode") != null) match {
    //docIds.map(id => searcher.storedFields().document(id)).find(doc1 => doc1.get("meshCode") != null) match {
      case Some(doc) => Right(doc.get("meshCode"))
      case None =>
        val labelSet: Set[String] = docIds.foldLeft(Set[String]()) {
          case (set, docId) =>
            searcher.storedFields().document(docId).getValues("termLabel").foldLeft(set) {
            //searcher.doc(docId).getValues("termLabel").foldLeft(set) {
              case (set1, value) => set1 + value.trim.toLowerCase
            }
        }
        Left(Some(labelSet))
    }
  }

  private def mesh2DeCS(code: String): Option[String] = {
    if (code.isEmpty) {
      None
    } else {
      thes2thesSearchers.get("DeCS") flatMap { searcher =>
        //println(s"code=$code searcher=$searcher")
        val ucCode = code.toUpperCase
        val uniCode = Tools.uniformString(code)
        val booleanQuery = new BooleanQuery.Builder()
          .add(new TermQuery(new Term("MESH_ID", ucCode)), BooleanClause.Occur.SHOULD)
          .add(new TermQuery(new Term("HIERARCHICAL_CODE", ucCode)), BooleanClause.Occur.SHOULD)
          .add(new TermQuery(new Term("ENGLISH_DESCR_NORM", uniCode)), BooleanClause.Occur.SHOULD)
          .add(new TermQuery(new Term("SPANISH_DESCR_NORM", uniCode)), BooleanClause.Occur.SHOULD)
          .add(new TermQuery(new Term("PORTUGUESE_DESCR_NORM", uniCode)), BooleanClause.Occur.SHOULD)
          .build()
        val topDocs = searcher.search(booleanQuery, 1)

        if (topDocs.totalHits.value == 0) {
          None
        } else {
          val doc = searcher.storedFields().document(topDocs.scoreDocs.head.doc)
          //val doc = searcher.doc(topDocs.scoreDocs.head.doc)
          val eDescr = doc.get("ENGLISH_DESCR")
          if (eDescr == null) {
            val sDescr = doc.get("SPANISH_DESCR")
            if (sDescr == null) {
              Option(doc.get("PORTUGUESE_DESCR"))
            } else {
              Some(sDescr)
            }
          } else {
            Some(eDescr)
          }
        }
      }
    }
  }
}
