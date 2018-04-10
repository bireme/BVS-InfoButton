/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter, Tools}
import scala.util.{Try, Success, Failure}

class SubTopic(
    codeSystem: Option[String] = Some("2.16.840.1.113883.6.177"),
    code: Option[String] = None,
    displayName: Option[String] = None,
    originalText: Option[String] = None) extends SearchParameter {
//println(s"SubTopic codeSystem=$codeSystem code=$code displayName=$displayName originalText=$originalText")
  assert(
    codeSystem.get.equals("2.16.840.1.113883.6.177") || // MeSH
      codeSystem.get.equals("2.16.840.1.113883.6.96")) // SNOMED CT
  assert((!code.isEmpty) || (!displayName.isEmpty) || (!originalText.isEmpty))
//println("SubTopic depois")
  private val snomedct2DeCS = Map(
    "404204005" -> "(\"drug-drug\" OR \"droga-droga\")",
    "95906008"  -> "(drug AND interaction AND alcohol)",
    "drug interaction with alcohol" -> "(drug AND interaction AND alcohol)"
  )

  override def toSrcExpression(conv: MeshConverter,
                               env: Seq[SearchParameter]): Option[String] = {
//println("===Entrei no toSrcExpression")
    val codeSrcExpr = code.flatMap(x => getSrcExpr(x, conv))
    val dplNameSrcExpr = displayName.flatMap(x => getSrcExpr(x, conv))
    val oTextSrcExpr = originalText.flatMap(x => getSrcExpr(x, conv))
  //println(s"codeSrcExpr=${codeSrcExpr.getOrElse("")} dplNameSrcExpr=${dplNameSrcExpr.getOrElse("")} oTextSrcExpr=${oTextSrcExpr.getOrElse("")}")
    val aux1 = codeSrcExpr.getOrElse("")
    val aux2 = if (dplNameSrcExpr.isEmpty) aux1
               else if (aux1.isEmpty) dplNameSrcExpr.get else s"OR ${dplNameSrcExpr.get}"
    val aux3 = if (oTextSrcExpr.isEmpty) aux2
               else if (aux2.isEmpty) oTextSrcExpr.get else s"OR ${oTextSrcExpr.get}"
    if (aux3.isEmpty) None else Some(Tools.encodeUrl(aux3))
  }

  private def getSrcExpr(in: String,
                         conv: MeshConverter): Option[String] = {
    val intr = in.trim
    val in2 = intr //if (intr.endsWith("*")) intr else intr + "*"
    val ostr: Option[String] = conv.convert(codeSystem.get, in2) match {
      case Right(str) => Some(str)
      case Left(optStr) => optStr orElse snomedct2DeCS.get(intr.toLowerCase)
    }
//println(s"ostr=$ostr")
    ostr.map {str => if (str(0) == '/') (true, str.substring(1))
                     else (false, str)
    }.map {
      case (qual, expr) =>
        val index = if (qual) "sh" else "mh"
        s"($index:${'"'}$expr${'"'} OR ti:($expr) OR ab:($expr))"
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
//println(s"SubTopic parameters=$parameters")
    val (sut, others) = parameters.partition(_._1.startsWith("subTopic"))
//println(s"SubTopic sut=$sut pthers=$others")
    if (sut.isEmpty) (Seq(), others)
    else {
      Try {
        new SubTopic(
          sut.get("subTopic.v.cs"),
          sut.get("subTopic.v.c"),
          sut.get("subTopic.v.dn"),
          sut.get("subTopic.v.ot"))
      } match {
        case Success(s) => (Seq(s), others)
        case Failure(x) => println(s"SubTopic failure=$x");(Seq(), others)
      }
    }
  }
}
