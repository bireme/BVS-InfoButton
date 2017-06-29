package org.bireme.infob

import org.bireme.infob.parameters._

object ParameterParser {
  val parSeq = Seq(
    AdministrativeGenderCode,
    Age,
    AgeGroup,
    InfoRecipient,
    Performer
  )

  def parse(param: Map[String,String]): (Seq[SearchParameter],
                                         Option[String],
                                         Option[String]) = {
    require(param != null)

    val (msc, others) = MainSearchCriteria.parse(param)

    val seqParam = parSeq.foldLeft[Seq[SearchParameter]] (msc) {
      case (seq,src) => seq ++ src.parse(others)
    }
    val responseType = others.get("knowledgeResponseType").map(_.toLowerCase)
    val callbackFunc = if (responseType.equals("application/javascript"))
      others.get("jsonp") else None

    (seqParam, responseType, callbackFunc)
  }
}

object ParameterParserTest extends App {
  val url =
    "representedOrganization.id.root=[OID of the organization submitting the" +
    "request]&taskContext.c.c=PROBLISTREV&mainSearchCriteria.v.c=E11&mainSea" +
    "rchCriteria.v.cs=2.16.840.1.113883.6.90&mainSearchCriteria.v.dn=Type+2+" +
    "Diabetes+Mellitus&mainSearchCriteria.v.ot=diabetes+type+2&patientPerson" +
    ".administrativeGenderCode.c=M&age.v.v=45&age.v.u=a&informationRecipient" +
    "=PAT&performer=PROV&performer.languageCode.c=en&performer.healthCarePro" +
    "vider.c.c=163W00000X&knowledgeResponseType=text/XML"

  val map = url.split("\\&").map(_.split("=")).
    foldLeft[Map[String,String]](Map()) {
      case (map,arr) => map + ((arr(0).trim,arr(1).trim))
    }
  map.foreach(kv => println(s"${kv._1} -> ${kv._2}"))
  println()

  val (params,responseType, callbackFunc) = ParameterParser.parse(map)

  params.foreach(p => println(p.toString + "\n"))
  println(s"""knowledgeResponseType=${responseType.getOrElse("application/json")}""")
  callbackFunc.map(s => println(s"callback function=$s"))
}
