package org.bireme.infob

class MeshConverter {
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

  private def icd9_cm2Mesh(code: String): Option[String] = None
  private def icd10_cm2Mesh(code: String): Option[String] = None
  private def icd102Mesh(code: String): Option[String] = None
  private def snomed_ct2Mesh(code: String): Option[String] = None
  private def rxnorm2Mesh(code: String): Option[String] = None
  private def ndc2Mesh(code: String): Option[String] = None
  private def loinc2Mesh(code: String): Option[String] = None
}
