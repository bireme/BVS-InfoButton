/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}
import scala.util.{Try, Success, Failure}

class SubTopic(
    codeSystem: Option[String] = Some("2.16.840.1.113883.6.177"),
    code: Option[String] = None,
    displayName: Option[String] = None,
    originalText: Option[String] = None) extends SearchParameter {
  assert(
    codeSystem.get.equals("2.16.840.1.113883.6.177") || // MeSH
      codeSystem.get.equals("2.16.840.1.113883.6.96")) // SNOMED CT
  assert((!code.isEmpty) || (!displayName.isEmpty) || (!originalText.isEmpty))

  private val meshCode2DeCS = Map(
    "Q000008" -> "/administration & dosage",
    "Q000744" -> "/contraindications",
    "Q000009" -> "/adverse effects",
    "D004347" -> "Drug Interactions",
    "Q000145" -> "/classification",
    "Q000209" -> "/etiology",
    "Q000175" -> "/diagnosis",
    "Q000628" -> "/therapy",
    "D011379" -> "Prognosis",
    "Q000627" -> "/therapeutic use",
    "Q000493" -> "/pharmacokinetics",
    "Q000494" -> "/pharmacology",
    "Q000633" -> "/toxicity",
    "Q000506" -> "/poisoning"
  )

  private val meshDisplayName2DeCS = Map(
    "administration & dosage" -> "/administration & dosage",
    "contraindications"       -> "/contraindications",
    "adverse effects"         -> "/adverse effects",
    "drug interaction"        -> "Drug Interactions",
    "classification"          -> "/classification",
    "etiology"                -> "/etiology",
    "diagnosis"               -> "/diagnosis",
    "therapy"                 -> "/therapy",
    "prognosis"               -> "Prognosis",
    "therapeutic use"         -> "/therapeutic use",
    "pharmacokinetics"        -> "/pharmacokinetics",
    "pharmacology"            -> "/pharmacology",
    "toxicity"                -> "/toxicity",
    "poisoning"               -> "/poisoning"
  )

  private val snomedctCode2DeCS = Map(
    "79899007"  -> "Drug Interactions",
    "47965005"  -> "Diagnosis, Differential",
    "404204005" -> "(\"drug-drug\" OR \"droga-droga\")",
    "95907004"  -> "Food-Drug Interactions"
    //"95906008"  -> ""  Drug interaction with alcohol
  )

  private val snomedctDisplayName2DeCS = Map(
    "drug interaction"           -> "Drug Interactions",
    "differential diagnosis"     -> "Diagnosis, Differential",
    "404204005"                  -> "(\"drug-drug\" OR \"droga-droga\")",
    "drug interaction with food" -> "Food-Drug Interactions"
    //"Drug interaction with alcohol" => ""
  )

  private def replaceSpaces(in: String): String =
    if (in == null) null else in.replace(" ", "%20")

  override def toSrcExpression(conv: MeshConverter,
                               env: Seq[SearchParameter]): Option[String] = {
//println("===Entrei no toSrcExpression")
    val ret = codeSystem match {
      case Some("2.16.840.1.113883.6.177") =>  // MeSH
        getSrcExpression(meshCode2DeCS, meshDisplayName2DeCS)
      case Some("2.16.840.1.113883.6.96") =>   // SNOMED CT
        getSrcExpression(snomedctCode2DeCS, snomedctDisplayName2DeCS)
      case _ => None
    }
//    println(s"ret=$ret")
    ret
  }

  private def getSrcExpression(codeMap: Map[String,String],
                               displayNameMap: Map[String, String]): Option[String] = {
    code match {
      case Some(c) =>
        codeMap.get(c.toUpperCase) match {
          case Some(expr) =>
            val field = if (expr contains " OR ") "ti" else "mh"
            Some(s"($field:(%22${replaceSpaces(expr)}%22))")
          case None =>
            displayName flatMap {
              dn =>
                displayNameMap.get(dn.toLowerCase) flatMap {
                  expr =>
                    val field = if (expr contains " OR ") "ti" else "mh"
                    Some(s"($field:(%22${replaceSpaces(expr)}%22))")
                }
            }
        }
      case _ =>
        displayName flatMap {
          dn =>
            displayNameMap.get(dn.toLowerCase).flatMap {
              expr =>
                val field = if (expr contains " OR ") "ti" else "mh"
                Some(s"($field:(%22${replaceSpaces(expr)}%22))")
            }
        }
    }
  }

  override def getCategories: Seq[Category] = {
    Seq(
      Category("subTopic.v.c", code.getOrElse("")),
      Category("subTopic.v.cs", codeSystem.get),
      Category("subTopic.v.dn", displayName.getOrElse("")),
      Category("subTopic.v.ot", originalText.getOrElse(""))
    ).filter(!_.term.isEmpty)
  }

  override def toString =
    s"""subTopic(codeSystem: Option[String] = $codeSystem,
                 code: Option[String] = $code,
                 displayName: Option[String] = $displayName,
                 originalText: Option[String] = $originalText)"""
}

object SubTopic extends Parser {
  override def parse(parameters: Map[String, String])
    :(Seq[SearchParameter], Map[String, String]) = {

    val (sut, others) = parameters.partition(_._1.startsWith("subTopic"))

    if (sut.isEmpty) (Seq(), others)
    else {
      Try (
        new SubTopic(
          parameters.get("subTopic.v.cs"),
          parameters.get("subTopic.v.c"),
          parameters.get("subTopic.v.dn"),
          parameters.get("subTopic.v.ot"))
      ) match {
        case Success(s) => (Seq(s), others)
        case Failure(_) => (Seq(), others)
      }
    }
  }
}
