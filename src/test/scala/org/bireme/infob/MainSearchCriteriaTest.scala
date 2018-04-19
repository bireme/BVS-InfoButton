/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

import java.net.URL
import org.scalatest._
import org.scalatest.concurrent.Timeouts._
import org.scalatest.Matchers._
import org.scalatest.time.SpanSugar._
import scala.io._

/** Application which uses ScalaTest to check the BVS-Infobutton's Main Search
* Criteria service parameter
*
* @author: Heitor Barbieri
* date: 20180418
*/
class MainSearchCriteriaTest extends FlatSpec {

  /* === Check if the service complains if no Main Search Criteria paramenter is
    used === */

  /* === Check if the service complains if Main Search Criteria (Concept Code)
  is used without specifying the Code System  === */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying an invalid the Code System  === */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System  (Mesh) === */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System (CID10)=== */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System  (CID10-CM)=== */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System  (CID9-CM)=== */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System  (Snomed-CT)=== */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System  (RXNORM)=== */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System  (NDC)=== */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System (LOINC)=== */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System  () === */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying an valid code  === */

  /* === Check the service result if Main Search Criteria (Display Name)
  is used with a Code System  === */

  /* === Check the service result if Main Search Criteria (Display Name)
  is specified === */

  /* === Check the service result if Main Search Criteria (Display Name)
  is specified with an invalid value === */

  /* === Check the service result if Main Search Criteria (Original Text)
  is used with a Code System  === */

  /* === Check the service result if Main Search Criteria (Original Text)
  is specified === */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used with (Display Name) - both specifying the same concept === */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used with (Display Name) - but not specifying the same concept === */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used with (Original Text) - both specifying the same concept === */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used with (Original Text) - but not specifying the same concept === */

  /* === Check the service result if Main Search Criteria (Display Name)
  is used with (Original Text) - both specifying the same concept === */

  /* === Check the service result if Main Search Criteria (Display Name)
  is used with (Original Text) - but not specifying the same concept === */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used with (Display Name) and (Original Text) - all specifying the same
  concept === */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used with (Display Name) and (Original Text) - but not specifying the
  same concept === */

  /* === Check the service result if Main Search Criteria1 (Concept Code1)
  is used only with (Code System2)  === */

  /* === Check the service result if Main Search Criteria1 (Concept Code1)
  is used specifying (Code System1) and (Code System2)  === */

  /* === Check the service result if Main Search Criteria1 (Concept Code1)
  and (Code System1) is used with Main Search Criteria2 (Concept Code2) and
  (Code System2) === */

  /* === Check the service result if Main Search Criteria1 (Concept Code1)
  and (Code System1) is used with  Main Search Criteria2 (Display Name2)=== */

  /* === Check the service result if Main Search Criteria1 (Concept Code1)
  and (Code System1) is used with  Main Search Criteria2 (Original Text2)=== */

  /* === Check the service result if Main Search Criteria1 (Display Name1)
  is used with Main Search Criteria2 (Code System2) and (Code System2)  === */

  /* === Check the service result if Main Search Criteria1 (Display Name1)
  is used with Main Search Criteria2 (Display Name2)  === */

  /* === Check the service result if Main Search Criteria1 (Display Name1)
  is used with Main Search Criteria2 (Original Text2)  === */

  /* === Check the service result if Main Search Criteria1 (Original Text1)
  is used with Main Search Criteria2 (Code System2) and (Code System2)  === */

  /* === Check the service result if Main Search Criteria1 (Original Text1)
  is used with Main Search Criteria2 (Display Name2)  === */

  /* === Check the service result if Main Search Criteria1 (Original Text1)
  is used with Main Search Criteria2 (Original Text2)  === */

  /**
    * Load the content of a web page and check if there is a Timeouts
    *
    * @param url the address to the page to be downloaded
    * @return the page content
    */
  private def pageContent(url:String): String = {
    require(url != null)

    val url2= new URL(url);
    /*val uri = new URI(url2.getProtocol(), url2.getUserInfo(), url2.getHost(),
                     url2.getPort(), url2.getPath(), url2.getQuery(), url2.getRef())
    val urlStr = uri.toASCIIString()*/

    var content = ""
    failAfter(60 seconds) {
      //val source = Source.fromURL(urlStr, "utf-8")
      val source = Source.fromURL(url2, "utf-8")
      content = source.getLines().mkString("\n")
      source.close()
    }

    content
  }

  // === Check if the The IAHx portal is available/accessible ===
  "The IAHx portal page" should "be available" in {
    val portal = "http://pesquisa.bvsalud.org/portal/"
    val div = """(?s)<div class="totalResults">(.+?)</div>""".r
    val totResults = "(?s)<strong>(.+?)</strong>".r
    val content = pageContent(portal)

    div.findFirstMatchIn(content) match {
      case Some(mat1) =>
        val totalResults = totResults.findAllMatchIn(mat1.group(1)).toArray.
                                      apply(1).group(1).replace(".", "").toInt
        totalResults should be > 26700000
      case None => fail
    }
  }

  // === Check if the IAHx Solr service is available/accessible ===
  "The IAHx Solr service" should "be available (dengue)" in {
    val url = "http://basalto02.bireme.br:8986/solr5/portal/select?q=tw:((instance:%22regional%22)%20AND%20(%20mh:(c02.081.270)))&start=0&rows=0"
    val result = """<result name="response" numFound="(\d+)" start="0">""".r
    val content = pageContent(url)

    result.findFirstMatchIn(content) match {
      case Some(mat) =>
        val numFound = mat.group(1).toInt
        numFound should be > 12500
      case None => fail
    }
  }

  // === Check if the IAHx Solr service is available/accessible ===
  "The IAHx Solr service" should "be available (zika neonatal)" in {
    val url = "http://basalto02.bireme.br:8986/solr5/portal/select?q=ti:(zika%20neonatal)%20AND%20(instance:%22regional%22)%20AND%20(%20year_cluster:(%222017%22))&start=0&rows=0"
    val result = """<result name="response" numFound="(\d+)" start="0">""".r
    val content = pageContent(url)

    result.findFirstMatchIn(content) match {
      case Some(mat) =>
        val numFound = mat.group(1).toInt
        numFound should be >= 4
      case None => fail
    }
  }
}
