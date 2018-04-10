/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

import org.bireme.infob.parameters._

/**
  * An object that convertes the map representing url parameters into
  * SearchParameters objects.
  *
  * @author: Heitor Barbieri
  */
object ParameterParser {
  // Sequence of search parameter classes the url parameters will be converted to
  val parSeq = Seq(
    "MainSearchCriteria",
    "AdministrativeGenderCode",
    "Age",
    "AgeGroup",
    "InfoRecipient",
    "Performer",
    "SubTopic",
    "LocationOfInterest"
  )

  /**
    * Convert a map of string url parameters into a sequence of search parameter
    * objects.
    *
    * @param param the map of url parameters like: &param=value
    * @return a triple of: 1) Sequence of search parameter classes
    *                      2) Possibly the return type format (xml,json,json-p)
    *                      3) Possibly the name of the javascript callback function
    */
  def parse(param: Map[String, String])
    : (Seq[SearchParameter], Option[String], Option[String]) = {
    require(param != null)

    val (spSeq, others) = parse(parSeq, param, Seq())
    val responseType = others.get("knowledgeResponseType").map(_.toLowerCase)
    val callbackFunc =
      if (responseType.equals("application/javascript")) others.get("jsonp")
      else None

    (spSeq, responseType, callbackFunc)
  }

  private def parse(names: Seq[String],
                    param: Map[String, String],
                    auxSrcParam: Seq[SearchParameter])
    : (Seq[SearchParameter], Map[String, String]) = {

    if (names.isEmpty) (auxSrcParam, param)
    else {
      val name = names.head
      val clazz = Class.forName("org.bireme.infob.parameters." + name + "$")
      //println(s"name=$clazz")
      val obj = clazz.getField("MODULE$").get(classOf[Parser]).asInstanceOf[Parser]
      val (seq, others) = {
        val (sp, oths) = obj.parse(param)
        if (sp.isEmpty) (auxSrcParam, oths) else (auxSrcParam ++ sp, oths)
      }
      //println(s"name=$clazz ${obj} => seq=$seq others=$others")
      parse(names.tail, others, seq)
    }
  }
}

/**
  * An application to test the ParameterParser object
  */
object ParameterParserTest extends App {
  val url =
    "representedOrganization.id.root=[OID of the organization submitting the" +
      "request]&taskContext.c.c=PROBLISTREV&mainSearchCriteria.v.c=E11&mainSea" +
      "rchCriteria.v.cs=2.16.840.1.113883.6.90&mainSearchCriteria.v.dn=Type+2+" +
      "Diabetes+Mellitus&mainSearchCriteria.v.ot=diabetes+type+2&patientPerson" +
      ".administrativeGenderCode.c=M&age.v.v=45&age.v.u=a&informationRecipient" +
      "=PAT&performer=PROV&performer.languageCode.c=en&performer.healthCarePro" +
      "vider.c.c=163W00000X&knowledgeResponseType=text/XML"

  val map =
    url.split("\\&").map(_.split("=")).foldLeft[Map[String, String]](Map()) {
      case (map, arr) => map + ((arr(0).trim, arr(1).trim))
    }
  map.foreach(kv => println(s"${kv._1} -> ${kv._2}"))
  println()

  val (params, responseType, callbackFunc) = ParameterParser.parse(map)

  params.foreach(p => println(p.toString + "\n"))
  println(
    s"""knowledgeResponseType=${responseType.getOrElse("application/json")}""")
  callbackFunc.map(s => println(s"callback function=$s"))
}
