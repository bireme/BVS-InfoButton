/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}

trait SearchParameter {
  def toSrcExpression(conv: MeshConverter): Option[String]
  def getCategories: Seq[Category]
}
