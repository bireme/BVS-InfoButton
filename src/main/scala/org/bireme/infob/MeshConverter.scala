/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

/**
  * Class to convert term codes from different thesaurus into correspondent
  * term codes of MESH thesaurus.
  *
  * @author Heitor Barbieri
  */
class MeshConverter {

  /**
    * Convert one term code from a particular thesaurus to the code of the
    * same term but in a different thesaurus
    *
    * @param codeSystem the code of the thesaurus name
    * @param code the term code
    * @return the other term code if the conversion is possible
    */
  def convert(codeSystem: String,
              code: String): Option[String] = {
    codeSystem.toUpperCase match {
      case "ICD9-CM" => icd9_cm2Mesh(code)
      case "2.16.840.1.113883.6.103" => icd9_cm2Mesh(code)
      case "ICD10-CM" => icd10_cm2Mesh(code)
      case "2.16.840.1.113883.6.90" => icd10_cm2Mesh(code)
      case "ICD10" => icd102Mesh(code)
      case "2.16.840.1.113883.6.3" => icd102Mesh(code)
      case "SNOMED-CT" => snomed_ct2Mesh(code)
      case "2.16.840.1.113883.6.96" => snomed_ct2Mesh(code)
      case "RXNORM" => rxnorm2Mesh(code)
      case "2.16.840.1.113883.6.88" => rxnorm2Mesh(code)
      case "MESH" => Some(code)
      case "2.16.840.1.113883.6.177" => Some(code)
      case "NDC" => ndc2Mesh(code)
      case "2.16.840.1.113883.6.69" => ndc2Mesh(code)
      case "LOINC" => loinc2Mesh(code)
      case "2.16.840.1.113883.6.1" => loinc2Mesh(code)
      case _ => None
    }
  }

  /**
    * Convert an IDC9_CM term code into a MESH term code
    * @param an IDC9_CM term code
    * @return a MESH term code if possible
    */
  private def icd9_cm2Mesh(code: String): Option[String] = None

  /**
    * Convert an IDC10_CM term code into a MESH term code
    * @param an IDC10_CM term code
    * @return a MESH term code if possible
    */
  private def icd10_cm2Mesh(code: String): Option[String] = None

  /**
    * Convert an IDC10 term code into a MESH term code
    * @param an IDC10 term code
    * @return a MESH term code if possible
    */
  private def icd102Mesh(code: String): Option[String] = None

  /**
    * Convert a SNOMED-CT term code into a MESH term code
    * @param a SNOMED-CT term code
    * @return a MESH term code if possible
    */
  private def snomed_ct2Mesh(code: String): Option[String] = None

  /**
    * Convert an IDC9_CM term code into a MESH term code
    * @param an IDC9_CM term code
    * @return a MESH term code if possible
    */
  private def rxnorm2Mesh(code: String): Option[String] = None

  /**
    * Convert a NDC term code into a MESH term code
    * @param a NDC term code
    * @return a MESH term code if possible
    */
  private def ndc2Mesh(code: String): Option[String] = None

  /**
    * Convert a LOINC term code into a MESH term code
    * @param a LONC term code
    * @return a MESH term code if possible
    */
  private def loinc2Mesh(code: String): Option[String] = None
}
