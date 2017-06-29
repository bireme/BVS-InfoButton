package org.bireme.infob.parameters

trait Parser {
  def parse(parameters: Map[String,String]): Option[SearchParameter]
}
