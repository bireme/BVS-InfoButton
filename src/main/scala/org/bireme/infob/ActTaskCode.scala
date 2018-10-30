/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob

/**
  *
  * Description: A task or action that a user may perform in a clinical information system.
  * Id: 2.16.840.1.113883.1.11.19846
  * Name: ActTaskCode
  * Version Label: DEFN=UV=VO=1099-20110726
  */
object ActTaskCode {
  val name = "ActTaskCode"
  val id = "2.16.840.1.113883.1.11.19846"

  val codeDisplyName = List(
    ("OE", "order entry task"),
    ("LABOE", "laboratory test order entry task"),
    ("MEDOE", "medication order entry task"),
    ("PATDOC", "patient documentation task"),
    ("ALLERLREV", "allergy list review"),
    ("CLINNOTEE", "clinical note entry task"),
    ("DIAGLISTE", "diagnosis list entry task"),
    ("DISCHSUME", "discharge summary entry task"),
    ("PATREPE", "pathology report entry task"),
    ("PROBLISTE", "problem list entry task"),
    ("RADREPE", "radiology report entry task"),
    ("IMMLREV", "immunization list review"),
    ("REMLREV", "reminder list review"),
    ("WELLREMLREV", "wellness reminder list review"),
    ("PATINFO", "patient information review task"),
    ("ALLERLE", "allergy list entry"),
    ("CLINNOTEREV", "clinical note review task"),
    ("DISCHSUMREV", "discharge summary review task"),
    ("DIAGLISTREV", "diagnosis list review task"),
    ("IMMLE", "immunization list entry"),
    ("LABRREV", "laboratory results review task"),
    ("MICRORREV", "microbiology results review task"),
    ("MLREV", "medication list review task"),
    ("MARWLREV", "medication administration record work list review task"),
    ("OREV", "orders review task"),
    ("PATREPREV", "pathology report review task"),
    ("PROBLISTREV", "problem list review task"),
    ("RADREPREV", "radiology report review task"),
    ("REMLE", "reminder list entry"),
    ("WELLREMLE", "wellness reminder list entry"),
    ("RISKASSESS", "risk assessment instrument task"),
    ("FALLRISK", "falls risk assessment instrument task")
  )
}
