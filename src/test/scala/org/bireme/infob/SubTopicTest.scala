/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

import org.bireme.infob.Tools._
import org.scalatest._
import org.scalatest.Matchers._

/** Application which uses ScalaTest to check the BVS-Infobutton's Main Search
* Criteria service parameter
*
* @author: Heitor Barbieri
* date: 20180503
*/
class SubTopicTest extends FlatSpec {
  //val server = "http://serverofi5.bireme.br:8180"
  val server = "http://localhost:8084"
  val service = s"$server/BVSInfoButton/infobutton/search"
if (1 > 0) {

  /* === Check if the service complains if Main Search Criteria (Concept Code)
  is used without specifying the Code System  === */
  "BVS_Infobutton" should "not return documents if R00.2 (palpitações)" +
  " Concept Code is used as Main Search Criteria with no Code System." in {
    val url = service + "?" + urlEncode("mainSearchCriteria.v.c=R00.2")
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be (0)
        hasStringPattern(content, "[Pp]alpita[çc][õo]es") should be (false)
        case None => println(s"url=$url");fail
      }
  }

}
}
