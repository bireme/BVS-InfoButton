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
* date: 20180418
*/
class MainSearchCriteriaTest extends FlatSpec {
  //val server = "http://bvsinfobutton.homolog.bvsalud.org"
  //val service = s"$server/infobutton/search"
  val server = "http://localhost:8084"
  val service = s"$server/BVSInfoButton/infobutton/search"

if (1 < 0) {
  /* === Check if the service complains if no Main Search Criteria paramenter is
    used === */
  "BVS_Infobutton" should "not return any document if no Main Search Criteria " +
  "paramenter is used." in {
    val url = service
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be (0)
      case None => fail
    }
  }

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

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying an invalid the Code System  === */
  "BVS_Infobutton" should "not return documents if R00.2 (palpitações in ICD10)" +
  " Concept Code is used as Main Search Criteria with Code System 'MESH'." in {
    val url = service + "?" + urlEncode(
      "mainSearchCriteria.v.c=R00.2&mainSearchCriteria.v.cs=2.16.840.1.113883.6.177")
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be (0)
        hasStringPattern(content, "[Pp]alpita[^s]+s") should be (false)
        case None => fail
      }
  }

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System  (Mesh) === */
  "BVS_Infobutton" should "return more than 50 documents if D003715 (dengue in MESH)" +
  " Concept Code is used as Main Search Criteria with Code System 'MESH'." in {
    val url = service + "?" +
      "mainSearchCriteria.v.c=D003715&mainSearchCriteria.v.cs=2.16.840.1.113883.6.177"
    getPageContent(url) match {
      case Some(content) =>
//println(s"num=${getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org")}")
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
        hasStringPattern(content, "[Dd]engue") should be (true)
        case None => fail
      }
  }

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System (CID10)=== */
  "BVS_Infobutton" should "return more than 50 documents if A90 (dengue in ICD10)" +
  " Concept Code is used as Main Search Criteria with Code System 'MESH'." in {
    val url = service + "?" +
      "mainSearchCriteria.v.c=A90&mainSearchCriteria.v.cs=2.16.840.1.113883.6.3"
    getPageContent(url) match {
      case Some(content) =>
//println(s"num=${getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org")}")
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
        hasStringPattern(content, "[Dd]engue") should be (true)
        case None => fail
      }
  }

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System  (CID10-CM)=== */
  "BVS_Infobutton" should "return more than 50 documents if Q89.9 (Congenital deformity NOS)" +
  " Concept Code is used as Main Search Criteria with Code System 'CID10-CM'." in {
    val url = service + "?" +
      "mainSearchCriteria.v.c=Q89.9&mainSearchCriteria.v.cs=2.16.840.1.113883.6.90"
    getPageContent(url) match {
      case Some(content) =>
//println(s"num=${getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org")}")
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
        hasStringPattern(content, "[Cc]ongenital") should be (true)
        case None => fail
      }
  }

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System  (CID9-CM)=== */
  "BVS_Infobutton" should "return more than 32 documents if 641.20 (Premature separation of placenta)" +
  " Concept Code is used as Main Search Criteria with Code System 'CID9-CM'." in {
    val url = service + "?" +
      "mainSearchCriteria.v.c=641.20&mainSearchCriteria.v.cs=2.16.840.1.113883.6.103"
    getPageContent(url) match {
      case Some(content) =>
//println(s"num=${getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org")}")
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 32
        hasStringPattern(content, "[Pp]remature") should be (true)
        hasStringPattern(content, "[Pp]lacenta") should be (true)
        case None => fail
      }
  }

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System  (Snomed-CT)=== */
  "BVS_Infobutton" should "return more than 50 documents if 38362002 (dengue in SNOMED_CT)" +
  " Concept Code is used as Main Search Criteria with Code System 'SNOMED_CT'." in {
    val url = service + "?" +
      "mainSearchCriteria.v.c=38362002&mainSearchCriteria.v.cs=2.16.840.1.113883.6.96"
    getPageContent(url) match {
      case Some(content) =>
//println(s"num=${getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org")}")
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
        hasStringPattern(content, "[Dd]engue") should be (true)
        case None => fail
      }
  }

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System  (RXNORM)=== */
  "BVS_Infobutton" should "return more than 50 documents if 1995 (Capsule in RXNORM)" +
  " Concept Code is used as Main Search Criteria with Code System 'RXNORM'." in {
    val url = service + "?" +
      "mainSearchCriteria.v.c=1995&mainSearchCriteria.v.cs=2.16.840.1.113883.6.88"
    getPageContent(url) match {
      case Some(content) =>
//println(s"num=${getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org")}")
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
        hasStringPattern(content, "[Cc]apsule") should be (true)
        case None => fail
      }
  }

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System  (NDC)=== */

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used specifying a valid the Code System (LOINC)=== */

  /* === Check the service result if Main Search Criteria (Display Name)
  is used with an invalid Code System  === */
  "BVS_Infobutton" should "not return documents if sardinha in MESH" +
  " DisplayName is used as Main Search Criteria with Code System 'ICD10'." in {
    val url = service +
      "?mainSearchCriteria.v.dn=sardinha&mainSearchCriteria.v.cs=2.16.840.1.113883.6.3"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be (0)
        hasStringPattern(content, "[Ss]ardinha") should be (false)
        case None => fail
      }
  }

  /* === Check the service result if Main Search Criteria (Display Name)
  is used with a valid Code System  === */
  "BVS_Infobutton" should "not return documents if 'sardinha'" +
  " Display Name is used as Main Search Criteria with Code System 'MESH'." in {
    val url = service +
      "?mainSearchCriteria.v.dn=sardinha&mainSearchCriteria.v.cs=2.16.840.1.113883.6.177"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be (0)
        case None => fail
      }
  }

  /* === Check the service result if Main Search Criteria (Display Name)
  is used with a valid Code System  === */
  "BVS_Infobutton" should "not return documents if 'dengue'" +
  " Display Name is used as Main Search Criteria with Code System 'MESH'." in {
    val url = service +
      "?mainSearchCriteria.v.dn=dengue&mainSearchCriteria.v.cs=2.16.840.1.113883.6.177"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be (0)
        case None => fail
      }
  }

  /* === Check the service result if Main Search Criteria (Display Name)
  is specified with an invalid value === */
  "BVS_Infobutton" should "no return documents if 'carona' (not in MESH)" +
  " Concept Code is used as Main Search Criteria with Code System 'MESH'." in {
    val url = service +
      "?mainSearchCriteria.v.dn=carona&mainSearchCriteria.v.cs=2.16.840.1.113883.6.177"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be (0)
        case None => fail
      }
  }

  /* === Check the service result if Main Search Criteria (Original Text)
  is used with a Code System  === */
  "BVS_Infobutton" should "return more than 50 documents if Main Search Criteria " +
  "'dengue' Original Test is used with 'MESH' Code System" +
  "Display Name - both specifying the same concept." in {
    val url = service +
      "?mainSearchCriteria.v.ot=dengue&mainSearchCriteria.v.cs=2.16.840.1.113883.6.177"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 3
        hasStringPattern(content, "[Dd]engue") should be (true)
        case None => fail
      }
  }

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used with (Display Name) - both specifying the same concept === */
  "BVS_Infobutton" should "return more than 50 documents if Main Search Criteria " +
  "'D003715' (MESH) Concept Code is used with Main Search Criteria 'dengue' " +
  "Display Name - both specifying the same concept." in {
    val url = service +
      "?mainSearchCriteria.v.c=D003715&mainSearchCriteria.v.cs=2.16.840.1.113883.6.177" +
      "&mainSearchCriteria.v.dn=dengue"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
        hasStringPattern(content, "[Dd]engue") should be (true)
        case None => fail
      }
  }

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used with (Display Name) - but not specifying the same concept === */
  "BVS_Infobutton" should "return more than 50 documents if Main Search Criteria " +
  "'D003715' (MESH) Concept Code is used with Main Search Criteria 'malaria' " +
  "Display Name - but not specifying the same concept." in {
    val url = service +
      "?mainSearchCriteria.v.c=D003715&mainSearchCriteria.v.cs=2.16.840.1.113883.6.177" +
      "&mainSearchCriteria.v.dn=malaria"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
        hasStringPattern(content, "[Dd]engue") should be (true)
        case None => fail
      }
  }

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used with (Original Text) - both specifying the same concept === */
  "BVS_Infobutton" should "return more than 50 documents if Main Search Criteria " +
  "'D002648 - criança' Code Concept in 'MESH' is used with Main Search Criteria " +
  "criança' Original Text. Both same concepts" in {
    val url = service + "?" +
      "mainSearchCriteria.v.c=D002648&" +
      "mainSearchCriteria.v.cs=2.16.840.1.113883.6.177&" +
      "mainSearchCriteria.v.ot=criança"
    getPageContent(url) match {
      case Some(content) =>
        hasStringPattern(content, "[Cc]rian[çc]a|[Cc]hild") should be (true)
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
      case None => fail
    }
  }

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used with (Original Text) - but not specifying the same concept === */
  "BVS_Infobutton" should "return more than 50 documents if Main Search Criteria " +
  "'D002648 - criança' Code Concept in 'MESH' is used with Main Search Criteria " +
  "mortalidade' Original Text. All different concepts" in {
    val url = service + "?" +
      "mainSearchCriteria.v.c=D002648&" +
      "mainSearchCriteria.v.cs=2.16.840.1.113883.6.177&" +
      "mainSearchCriteria.v.ot=mortalidade"
    getPageContent(url) match {
      case Some(content) =>
        hasStringPattern(content, "[Cc]rian[çc]a|[Cc]hild") should be (true)
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
      case None => fail
    }
  }

  /* === Check the service result if Main Search Criteria (Display Name)
  is used with (Original Text) - both specifying the same concept === */
  "BVS_Infobutton" should "return more than 50 documents if Main Search Criteria " +
  "Main Search Criteria 'dengue' Display Name is used with 'dengue' Original Text." +
  " All same concepts" in {
    val url = service + "?" +
      "mainSearchCriteria.v.dn=dengue&mainSearchCriteria.v.ot=dengue"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
        hasStringPattern(content, "[Dd]engue") should be (true)
      case None => fail
    }
  }

  /* === Check the service result if Main Search Criteria (Display Name)
  is used with (Original Text) - but not specifying the same concept === */
  "BVS_Infobutton" should "return more than 50 documents if Main Search Criteria " +
  "Main Search Criteria 'dengue' Display Name is used with 'criança' Original Text." +
  " All different concepts" in {
    val url = service + "?" +
      "mainSearchCriteria.v.dn=dengue&mainSearchCriteria.v.ot=criança"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
        hasStringPattern(content, "[Dd]engue") should be (true)
      case None => fail
    }
  }

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used with (Display Name) and (Original Text) - all specifying the same
  concept === */
  "BVS_Infobutton" should "return more than 50 documents if Main Search Criteria " +
  "'D002648 - criança' Code Concept in 'MESH' is used with Main Search Criteria " +
  "'criança' Display Name and 'criança' Original Text. All different concepts" in {
    val url = service + "?" +
      "mainSearchCriteria.v.c=D002648&" +
      "mainSearchCriteria.v.cs=2.16.840.1.113883.6.177&" +
      "mainSearchCriteria.v.dn=criança&mainSearchCriteria.v.ot=criança"
    getPageContent(url) match {
      case Some(content) =>
        hasStringPattern(content, "[Cc]rian[çc]a|[Cc]hild") should be (true)
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
      case None => fail
    }
  }

  /* === Check the service result if Main Search Criteria (Concept Code)
  is used with (Display Name) and (Original Text) - but not specifying the
  same concept === */
  "BVS_Infobutton" should "return more than 50 documents if Main Search Criteria " +
  "'D002648 - criança' Code Concept in 'MESH' is used with Main Search Criteria " +
  "'dengue' Display Name and 'mortalidade' Original Text. All different concepts" in {
    val url = service + "?" +
      "mainSearchCriteria.v.c=D002648&" +
      "mainSearchCriteria.v.cs=2.16.840.1.113883.6.177&" +
      "mainSearchCriteria.v.dn=dengue&mainSearchCriteria.v.ot=mortalidade"
    getPageContent(url) match {
      case Some(content) =>
        hasStringPattern(content, "[Cc]rian[çc]a|[Cc]hild") should be (true)
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
      case None => fail
    }
  }

  /* === Check the service result if Main Search Criteria1 (Concept Code1)
  is used only with (Code System2)  === */
  "BVS_Infobutton" should "not return documents if Main Search Criteria1 " +
  "'D002648 - criança' Code Concept in 'MESH' but not specifying the Code System " +
  "is used with Main Search Criteria2 Code System 'MESH'" in {
    val url = service + "?" +
      "mainSearchCriteria.v.c0=D002648&" +
      "mainSearchCriteria.v.cs1=2.16.840.1.113883.6.177"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be (0)
      case None => fail
    }
  }

  /* === Check the service result if Main Search Criteria1 (Concept Code1)
  is used specifying (Code System1) and (Code System2)  === */
  "BVS_Infobutton" should "return more than 50 documents if Main Search Criteria1 " +
  "'D002648 - criança' Code Concept in 'MESH' Code System is used with Main " +
  "Search Criteria2 Code System '2.16.840.1.113883.6.96 - SNOMED-CT'" in {
    val url = service + "?" +
      "mainSearchCriteria.v.c0=D002648&" +
      "mainSearchCriteria.v.cs0=2.16.840.1.113883.6.177&" +
      "mainSearchCriteria.v.cs1=2.16.840.1.113883.6.96"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
        hasStringPattern(content, "[Cc]rian[çc]a|[Cc]hild") should be (true)
      case None => fail
    }
  }

  /* === Check the service result if Main Search Criteria1 (Concept Code1)
  and (Code System1) is used with Main Search Criteria2 (Concept Code2) and
  (Code System2) === */
  "BVS_Infobutton" should "return more than 50 documents if Main Search Criteria1 " +
  "'D002648 - criança' Code Concept in 'MESH' Code System is used with Main " +
  "Search Criteria2 'D009026 - mortalidade' Code Concept in 'MESH'" in {
    val url = service + "?" +
      "mainSearchCriteria.v.c0=D002648&" +
      "mainSearchCriteria.v.cs0=2.16.840.1.113883.6.177&mainSearchCriteria.v.c1=D009026&" +
      "mainSearchCriteria.v.cs1=2.16.840.1.113883.6.177"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
        hasStringPattern(content, "[Cc]rian[çc]a|[Cc]hild") should be (true)
        hasStringPattern(content, "[Mm]ortalid") should be (true)
      case None => fail
    }
  }

  /* === Check the service result if Main Search Criteria1 (Concept Code1)
  and (Code System1) is used with  Main Search Criteria2 (Display Name2)=== */
  "BVS_Infobutton" should "return more than 50 documents if Main Search Criteria1 " +
  "'D002648 - criança' Code Concept in 'MESH' Code System is used with Main " +
  "Search Criteria2 'mortalidade' Display Name" in {
    val url = service + "?" +
      "mainSearchCriteria.v.c0=D002648&" +
      "mainSearchCriteria.v.cs0=2.16.840.1.113883.6.177&mainSearchCriteria.v.dn1=mortalidade"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
        hasStringPattern(content, "[Cc]rian[çc]a|[Cc]hild") should be (true)
        hasStringPattern(content, "[Mm]ortalid") should be (true)
      case None => fail
    }
  }

  /* === Check the service result if Main Search Criteria1 (Concept Code1)
  and (Code System1) is used with  Main Search Criteria2 (Original Text2)=== */
  "BVS_Infobutton" should "return more than 50 documents if Main Search Criteria1 " +
  "'D002648 - criança' Code Concept in 'MESH' Code System is used with Main " +
  "Search Criteria2 'mortalidade' Original Text" in {
    val url = service + "?" +
      "mainSearchCriteria.v.c0=D002648&" +
      "mainSearchCriteria.v.cs0=2.16.840.1.113883.6.177&mainSearchCriteria.v.ot1=mortalidade"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
        hasStringPattern(content, "[Cc]rian[çc]a|[Cc]hild") should be (true)
        hasStringPattern(content, "[Mm]ortalid") should be (true)
      case None => fail
    }
  }

  /* === Check the service result if Main Search Criteria1 (Display Name1)
  is used with Main Search Criteria2 (Code Concept2) and (Code System2)  === */
  "BVS_Infobutton" should "return more than 40 documents if Main Search Criteria1 " +
  "'dengue' Display Name is used with Main Search Criteria2 'D002648 -criança' " +
  "Code Concept with 'MESH' Code System" in {
    val url = service + "?" +
      "mainSearchCriteria.v.dn0=dengue&mainSearchCriteria.v.c1=D002648&" +
      "mainSearchCriteria.v.cs1=2.16.840.1.113883.6.177"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 40
        hasStringPattern(content, "[Dd]engue") should be (true)
        hasStringPattern(content, "[Cc]rian[çc]a|[Cc]hild") should be (true)
      case None => fail
    }
  }

  /* === Check the service result if Main Search Criteria1 (Display Name1)
  is used with Main Search Criteria2 (Display Name2)  === */
  "BVS_Infobutton" should "not return documents if Main Search Criteria1 " +
  "'dengue' Display Name is used with Main Search Criteria2 'criança' Display Name" in {
    val url = service + "?" +
      "mainSearchCriteria.v.dn0=dengue&mainSearchCriteria.v.dn1=criança"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be (0)
      case None => fail
    }
  }

  /* === Check the service result if Main Search Criteria1 (Display Name1)
  is used with Main Search Criteria2 (Original Text2)  === */
  "BVS_Infobutton" should "return more than 40 documents if Main Search Criteria1 " +
  "'dengue' Display Name is used with Main Search Criteria2 'criança' Original Text" in {
    val url = service + "?" +
      "mainSearchCriteria.v.dn0=dengue&mainSearchCriteria.v.ot1=criança"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 40
        hasStringPattern(content, "[Dd]engue") should be (true)
        hasStringPattern(content, "[Cc]rian[çc]a") should be (true)
      case None => fail
    }
  }

  /* === Check the service result if Main Search Criteria1 (Original Text1)
  is used with Main Search Criteria2 (Code System2)  === */
  "BVS_Infobutton" should "return more than 50 documents if Main Search Criteria1 " +
  "'dengue' Original Text is used with Main Search Criteria2 'ICD10' Code System" in {
    val url = service + "?" +
      "mainSearchCriteria.v.ot0=dengue&mainSearchCriteria.v.cs1=2.16.840.1.113883.6.3"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 50
        hasStringPattern(content, "[Dd]engue") should be (true)
      case None => fail
    }
  }

  /* === Check the service result if Main Search Criteria1 (Original Text1)
  is used with Main Search Criteria2 (Display Name2)  === */
  "BVS_Infobutton" should "not return documents if Main Search Criteria1 " +
  "'dengue' Original Text is used with Main Search Criteria2 'criança' Display" +
  " Name" in {
    val url = service + "?" + "mainSearchCriteria.v.ot0=dengue&mainSearchCriteria.v.dn1=criança"
    //println(s"url=[$url]")
    getPageContent(url) match {
      case Some(content) =>
//println(s"url=[$url] content=[$content]")
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be (0)
      case None => fail
    }
  }

  /* === Check the service result if Main Search Criteria1 (Original Text1)
  is used with Main Search Criteria2 (Original Text2)  === */
  "BVS_Infobutton" should "return more than 40 documents if Main Search Criteria1 " +
  "'dengue' Original Text is used with Main Search Criteria2 'criança' with " +
  "Original Text" in {
    val url = service +
      "?mainSearchCriteria.v.ot0=dengue&mainSearchCriteria.v.ot1=criança"
    getPageContent(url) match {
      case Some(content) =>
        getNumberOccurrences(content, "\\<id\\>tag:bvsalud.org") should be >= 40
        hasStringPattern(content, "[Dd]engue") should be (true)
        hasStringPattern(content, "[Cc]rian[çc]a") should be (true)
      case None => fail
    }
  }
}
}
