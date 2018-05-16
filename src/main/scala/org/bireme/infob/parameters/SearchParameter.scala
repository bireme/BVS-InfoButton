/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.Category

trait SearchParameter {
  def toSrcExpression(env: Seq[SearchParameter]): Option[String]
  def getCategories: Seq[Category]
}
