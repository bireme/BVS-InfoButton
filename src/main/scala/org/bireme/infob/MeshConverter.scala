/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

import java.io.File

import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.index.{DirectoryReader,Term}
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{BooleanClause,BooleanQuery,IndexSearcher,TermQuery}
import org.apache.lucene.store.FSDirectory

import scala.util.{Either,Left,Right}

/**
  * Class to convert term codes from different thesaurus into correspondent
  * term codes of MESH thesaurus.
  *
  * @author Heitor Barbieri
  */
class MeshConverter(indexes: String) {
  // Names of Lucene indexes to convert one thesaurus into another
  val thes2thes = Map(
    "DeCS" -> s"$indexes/DeCS",
    "ICD10" -> s"$indexes/ICD10"
  )

  // Lucene index searchers to convert one system into another
  val thes2thesSearchers = thes2thes.map {
    case (k,v) =>
      val directory = FSDirectory.open(new File(v).toPath())
      val ireader = DirectoryReader.open(directory)
      (k, new IndexSearcher(ireader))
  }

  val analyzer = new KeywordAnalyzer()

  /**
    * Convert one term code from a particular thesaurus to the code of the
    * same term but in a different thesaurus
    *
    * @param codeSystem the code of the thesaurus name
    * @param code the term code
    * @return the a sequence of term codes (right) if the conversion is possible
    *         or the string describing the term associated with the code (left)
    */
  def convert(codeSystem: String,
              code: String): Either[Option[String],Set[String]] = {
    val tcode = code.trim.toUpperCase

    val mesh = codeSystem.trim.toUpperCase match {
      case "2.16.840.1.113883.6.103" => getMeshCodes("ICD9-CM", tcode)
      case "2.16.840.1.113883.6.90" => getMeshCodes("ICD10-CM", tcode)
      case "2.16.840.1.113883.6.3" => getMeshCodes("ICD10", tcode)
      case "2.16.840.1.113883.6.96" => getMeshCodes("SNOMED-CT", tcode)
      case "2.16.840.1.113883.6.88" => getMeshCodes("RXNORM", tcode)
      case "2.16.840.1.113883.6.177" => Right(Set(tcode))
      case "2.16.840.1.113883.6.69" => getMeshCodes("NDC", tcode)
      case "2.16.840.1.113883.6.1" => getMeshCodes("LOINC", tcode)
      case _ => Left(None)
    }

    // Try converting MeSH code or term into a DeCs code or term description
println(s"mesh=$mesh")
    mesh match {
      case Right(codes) =>
        val cset = codes.map(mesh2DeCS(_)).flatten
        println(s"cset=$cset")
        if (cset.isEmpty) Left(None)
        else Right(cset)
      case Left(x) => Left(x)
    }
  }

  private def getMeshCodes(codeSystem: String,
                           code: String): Either[Option[String],Set[String]] = {
    thes2thesSearchers.get(codeSystem) match {
      case Some(searcher) =>
println(s"codeSystem=$codeSystem code=$code searcher=$searcher")
        val parser = new QueryParser(codeSystem, analyzer)
        val query = parser.parse(code)
        val topDocs = searcher.search(query, 1)
println(s"query=$query totalHits=${topDocs.totalHits}")
        if (topDocs.totalHits == 0) Left(None)
        else {
          val doc = searcher.doc(topDocs.scoreDocs.head.doc)
          val meshCodes = doc.getValues("MeSH")
println(s"meshCodes=$meshCodes")
          if (meshCodes == null) {
            val meshDesc = doc.get("description")
            if (meshDesc == null) Left(None) else Left(Some(meshDesc))
          } else Right(meshCodes.toSet)
        }
      case None => Left(None)
    }
  }

  private def mesh2DeCS(code: String): Option[String] = {
    if (code.isEmpty) None
    else thes2thesSearchers.get("DeCS") flatMap {
      searcher =>
       println(s"code=$code searcher=$searcher")
        val booleanQuery = new BooleanQuery.Builder()
        .add(new TermQuery(new Term("MESH_ID", code.toUpperCase)),
             BooleanClause.Occur.SHOULD)
        .add(new TermQuery(new Term("HIERARCHICAL_CODE", code.toUpperCase)),
             BooleanClause.Occur.SHOULD)
        .add(new TermQuery(new Term("ENGLISH_DESCR_NORM", code.toUpperCase)),
             BooleanClause.Occur.SHOULD)
        .add(new TermQuery(new Term("SPANISH_DESCR_NORM", code.toUpperCase)),
             BooleanClause.Occur.SHOULD)
        .add(new TermQuery(new Term("PORTUGUESE_DESCR_NORM", code.toUpperCase)),
             BooleanClause.Occur.SHOULD)
        .build()

        val topDocs = searcher.search(booleanQuery, 1)
        if (topDocs.totalHits == 0) None
        else {
          println(s"Achou algum documento. totalHits=${topDocs.totalHits}")
          val doc = searcher.doc(topDocs.scoreDocs.head.doc)
          println(s"doc=$doc")
          val eDescr = doc.get("ENGLISH_DESCR")
          if (eDescr == null) {
            val sDescr = doc.get("SPANISH_DESCR")
            if (sDescr == null) Option(doc.get("PORTUGUESE_DESCR"))
            else Some(sDescr)
          } else Some(eDescr)
        }
    }
  }
}
