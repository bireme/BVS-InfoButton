/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter, Tools}
import scala.util.{Try, Success, Failure}

class LocationOfInterest(
    codeSystem: Option[String] = Some("2.16.840.1.113883.5.16"),
    conceptCode: Option[String] = Some("CNT"),
    code: Option[String] = None,
    displayName: Option[String] = None) extends SearchParameter {
  assert(codeSystem.get.equals("2.16.840.1.113883.5.16"))  // Code System AddressPartType
  assert((!conceptCode.isEmpty) || (!code.isEmpty) || (!displayName.isEmpty))

  val display2iAHx = ISO3166.codes1  // "Afghanistan" -> "asia",
  val code2iAHx = ISO3166.codes2     // "AFG" -> "asia",

  val iahxCode = conceptCode match {
    case Some("CNT") =>
      code match {
        case Some(icode) =>
          code2iAHx.get(icode) match {
            case Some(iahcode) =>
              if (iahcode.isEmpty) displayName.flatMap(dn => display2iAHx.get(dn))
              else Some(iahcode)
            case None => displayName.flatMap(dn => display2iAHx.get(dn))
          }
        case None => displayName.flatMap(dn => display2iAHx.get(dn))
      }
    case _ => None
  }

  override def toSrcExpression(conv: MeshConverter,
                               env: Seq[SearchParameter]): Option[String] = {
//println("===Entrei no toSrcExpression")
    iahxCode.map(ic => s"(pais_assunto:${'"'}${Tools.encodeUrl(ic)}${'"'})")
  }

  override def getCategories: Seq[Category] = {
    Seq(Category(s"locationOfInterest.addr.${conceptCode.get}",
      code.orElse(displayName).get))
  }

  override def toString =
    s"""locationOfInterest(codeSystem: Option[String] = $codeSystem,
                           conceptCode: Option[String] = $conceptCode,
                           code: Option[String] = $code,
                           displayName: Option[String] = $displayName)"""
}

object LocationOfInterest extends Parser {
  override def parse(parameters: Map[String, String])
    : (Seq[SearchParameter], Map[String, String]) = {
    val (loi, others) =
      parameters.partition(_._1.startsWith("locationOfInterest.addr."))

    (getLoI(loi, 0, Seq()), others)
  }

  private def getLoI(loi: Map[String, String],
                     cardinality: Int,
                     auxSeq: Seq[LocationOfInterest]): Seq[SearchParameter] = {
//println(s"*** getLoI loi=$loi cardinality=$cardinality auxSeq=$auxSeq")

    val cardi = if (cardinality == 0) "" else cardinality.toString
    val (loiCard, other) = loi.partition(
      kv => kv._1 matches s"locationOfInterest.addr.(ZIP|CTY|STA|CNT)$cardi"
    )
    if (loiCard.isEmpty) auxSeq
    else {
      val kv = loiCard.head
      val conceptCode = Some(kv._1.split("\\.")(2))
      val (code, displayName) =
        if (kv._2.size == 3) (Some(kv._2), None) else (None, Some(kv._2))
      val newAuxSeq = Try (
        new LocationOfInterest(conceptCode = conceptCode,
                               code = code,
                               displayName = displayName)
        ) match {
          case Success(s) => auxSeq :+ s
          case Failure(_) => auxSeq
        }

      getLoI(other, cardinality + 1, newAuxSeq)
    }
  }
}
