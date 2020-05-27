/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

import java.net.URL

import org.scalatest.matchers.should.Matchers._
import org.scalatest.concurrent.TimeLimits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.time.SpanSugar._

import scala.io._
import scala.util.matching.Regex

/** Application which uses ScalaTest to check IAHx portal and service
*
* author: Heitor Barbieri
* date: 20170808
*/
class IAHxServiceTest extends AnyFlatSpec {

  /**
    * Load the content of a web page and check if there is a Timeouts
    *
    * @param url the address to the page to be downloaded
    * @return the page content
    */
  private def pageContent(url:String): String = {
    require(url != null)

    val url2 = new URL(url)
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
    val portal = "https://pesquisa.bvsalud.org/portal/?output=xml&lang=pt&from=0&sort=&format=summary"
    val totalResults: Regex = "<result name=\"response\" numFound=\"(\\d+)\" start=\"0\">".r
    val content = pageContent(portal)

    totalResults.findFirstMatchIn(content) match {
      case Some(mat) =>
        val totalRes = mat.group(1).toInt
        totalRes should be > 26700000
      case None => fail
    }
  }

  // === Check if the IAHx Solr service is available/accessible ===
  "The IAHx Solr service" should "be available (dengue)" in {
    //val url = "http://basalto02.bireme.br:8986/solr5/portal/select?q=tw:((instance:%22regional%22)%20AND%20(%20mh:(c02.081.270)))&start=0&rows=0"
    val url = "http://basalto02.bireme.br:8986/solr5/portal/select?q=tw:((instance:%22regional%22)%20AND%20(%20mh:(C01.920.500.270)))&start=0&rows=0"
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
