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

/** Application which uses ScalaTest to check IAHx portal and service
*
* @author: Heitor Barbieri
* date: 20170808
*/
class IAHxTest extends FlatSpec {

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

  // === Check if the Similar Documents applet is available/accessible ===
  "The IAHx Solr service" should "be available" in {
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
}
