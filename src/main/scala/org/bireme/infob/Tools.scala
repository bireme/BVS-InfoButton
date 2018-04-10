/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

import java.net.URLEncoder
import java.text.Normalizer
import java.text.Normalizer.Form

object Tools {

  /**
    * Converts all input charactes into a-z, 0-9, '_' or eliminate them
    *
    * @param in input string to be converted
    * @return the converted string
    */
  def uniformString(in: String): String = {
    require(in != null)

    val s1 = Normalizer.normalize(in.trim().toLowerCase(), Form.NFD)
    val s2 = s1.replaceAll("[\\p{InCombiningDiacriticalMarks}]", "")

    s2.replaceAll("[^\\w]", "")
  }

  def replaceSpaces(in: String): String =
    if (in == null) null else in.replace(" ", "%20")

  def encodeUrl(in: String): String =  URLEncoder.encode(in, "UTF-8")    
}
