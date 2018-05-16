/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.MeshConverter

trait Parser {
  def parse(conv: MeshConverter,
            parameters: Map[String, String]):
    (Seq[SearchParameter], Map[String, String])
}
