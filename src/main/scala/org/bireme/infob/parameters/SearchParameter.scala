package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}

trait SearchParameter {
  def toSrcExpression(conv: MeshConverter): Option[String]
  def getCategories: Seq[Category]
}
