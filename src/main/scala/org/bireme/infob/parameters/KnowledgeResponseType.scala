/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}

class KnowledgeResponseType(krtype: Option[String],
                            callback: Option[String]) extends SearchParameter {
  val ktype = krtype match {
    case Some("text/xml") => "text/xml"
    case Some("application/json") => "application/json"
    case Some("application/javascript") => "application/javascript"
    case _ => "text/xml"
  }

  override def toSrcExpression(env: Seq[SearchParameter]): Option[String] = None

  override def getCategories: Seq[Category] =
    callback match {
      case Some(call) =>
        Seq(Category("knowledgeResponseType", ktype), Category("callback", call))
      case None => Seq(Category("knowledgeResponseType", ktype))
    }

  override def toString =
    s"""KnowledgeResponseType(krtype: Option[String] = $krtype,
                              callback: Option[String] = $callback)"""
}

object KnowledgeResponseType extends Parser {
  override def parse(conv: MeshConverter,
                     parameters: Map[String, String])
    :(Seq[SearchParameter], Map[String, String]) = {

    val (krt, others1) = parameters.partition(_._1.equals("knowledgeResponseType"))
    val (call, others2) = parameters.partition(_._1.equals("callback"))

    if (krt.isEmpty) (Seq(), others1)
    else {
      val krt2 = krt.head._2
      if (call.isEmpty) (Seq(new KnowledgeResponseType(Some(krt2), None)), others2)
      else (Seq(new KnowledgeResponseType(Some(krt2), Some(call.head._2))), others2)
    }
  }
}
