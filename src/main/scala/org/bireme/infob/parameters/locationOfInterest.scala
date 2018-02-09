/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

import org.bireme.infob.{Category, MeshConverter}
import scala.util.{Try, Success, Failure}

class LocationOfInterest(
    codeSystem: Option[String] = Some("2.16.840.1.113883.5.16"),
    conceptCode: Option[String] = Some("CNT"),
    code: Option[String] = None,
    displayName: Option[String] = None) extends SearchParameter {
  assert(codeSystem.get.equals("2.16.840.1.113883.5.16"))  // Code System AddressPartType
  assert((!conceptCode.isEmpty) || (!code.isEmpty) || (!displayName.isEmpty))

  val display2iAHx = Map(
    "Afghanistan" -> "asia",
    "Albania" -> "europa",
    "Algeria" -> "",
    "American Samoa" -> "",
    "Andorra" -> "europa",
    "Angola" -> "africa",
    "Anguilla" -> "anguilla",
    "Antarctica" -> "",
    "Antigua and Barbuda" -> "caribe",
    "Argentina" -> "argentina",
    "Armenia" -> "europa",
    "Aruba" -> "caribe",
    "Australia" -> "oceania",
    "Austria" -> "europa",
    "Azerbaijan" -> "asia",
    "Bahamas" -> "caribe",
    "Bahrain" -> "",
    "Bangladesh" -> "asia",
    "Barbados" -> "barbados",
    "Belarus" -> "europa",
    "Belgium" -> "europa",
    "Belize" -> "belize",
    "Benin" -> "",
    "Bermuda" -> "bermudas",
    "Bhutan" -> "",
    "Bolivia, Plurinational State of" -> "bolivia",
    "Bonaire, Sint Eustatius and Saba" -> "",
    "Bosnia and Herzegovina" -> "europa",
    "Botswana" -> "",
    "Bouvet Island" -> "",
    "Brazil" -> "brasil",
    "British Indian Ocean Territory" -> "",
    "Brunei Darussalam" -> "",
    "Bulgaria" -> "europa",
    "Burkina Faso" -> "",
    "Burundi" -> "",
    "Cabo Verde" -> "africa",
    "Cambodia" -> "asia",
    "Cameroon" -> "",
    "Canada" -> "america-do-norte",
    "Cayman Islands" -> "islas-caiman",
    "Central African Republic" -> "",
    "Chad" -> "",
    "Chile" -> "chile",
    "China" -> "asia",
    "Christmas Island" -> "",
    "Cocos (Keeling) Islands" -> "",
    "Colombia" -> "colombia",
    "Comoros" -> "",
    "Congo" -> "africa",
    "Congo, the Democratic Republic of the" -> "africa",
    "Cook Islands" -> "",
    "Costa Rica" -> "costa-rica",
    "Côte d'Ivoire" -> "",
    "Croatia" -> "europa",
    "Cuba" -> "cuba",
    "Curaçao" -> "caribe",
    "Cyprus" -> "",
    "Czechia" -> "europa",
    "Denmark" -> "europa",
    "Djibouti" -> "",
    "Dominica" -> "dominica",
    "Dominican Republic" -> "republica-dominicana",
    "Ecuador" -> "ecuador",
    "Egypt" -> "africa",
    "El Salvador" -> "el-salvador",
    "Equatorial Guinea" -> "africa",
    "Eritrea" -> "",
    "Estonia" -> "europa",
    "Ethiopia" -> "africa",
    "Falkland Islands (Malvinas)" -> "",
    "Faroe Islands" -> "europa",
    "Fiji" -> "oceania",
    "Finland" -> "europa",
    "France" -> "europa",
    "French Guiana" -> "guyana-francesa",
    "French Polynesia" -> "",
    "French Southern Territories" -> "",
    "Gabon" -> "",
    "Gambia" -> "",
    "Georgia" -> "europa",
    "Germany" -> "europa",
    "Ghana" -> "africa",
    "Gibraltar" -> "europa",
    "Greece" -> "europa",
    "Greenland" -> "america-do-norte",
    "Grenada" -> "grenada",
    "Guadeloupe" -> "guadalope",
    "Guam" -> "",
    "Guatemala" -> "guatemala",
    "Guernsey" -> "",
    "Guinea-Bissau" -> "",
    "Guinea" -> "",
    "Guyana" -> "guyana",
    "Haiti" -> "haiti",
    "Heard Island and McDonald Islands" -> "",
    "Holy See" -> "",
    "Honduras" -> "honduras",
    "Hong Kong" -> "asia",
    "Hungary" -> "europa",
    "Iceland" -> "europa",
    "India" -> "asia",
    "Indonesia" -> "asia",
    "Iran, Islamic Republic of" -> "asia",
    "Iraq" -> "asia",
    "Ireland" -> "europa",
    "Islands" -> "",
    "Isle of Man" -> "",
    "Israel" -> "asia",
    "Italy" -> "europa",
    "Jamaica" -> "jamaica",
    "Japan" -> "",
    "Jersey" -> "",
    "Jordan" -> "asia",
    "Kazakhstan" -> "asia",
    "Kenya" -> "africa",
    "Kiribati" -> "",
    "Korea, Democratic People's Republic of" -> "asia",
    "Korea, Republic of" -> "asia",
    "Kuwait" -> "asia",
    "Kyrgyzstan" -> "",
    "Lao People's Democratic Republic" -> "",
    "Latvia" -> "",
    "Lebanon" -> "asia",
    "Lesotho" -> "",
    "Liberia" -> "africa",
    "Libya" -> "africa",
    "Liechtenstein" -> "europa",
    "Lithuania" -> "europa",
    "Luxembourg" -> "europa",
    "Macao" -> "europa",
    "Macedonia, the former Yugoslav Republic of" -> "europa",
    "Madagascar" -> "africa",
    "Malawi" -> "",
    "Malaysia" -> "asia",
    "Maldives" -> "",
    "Mali" -> "",
    "Malta" -> "europa",
    "Marshall Islands" -> "oceania",
    "Martinique" -> "martinica",
    "Mauritania" -> "africa",
    "Mauritius" -> "",
    "Mayotte" -> "",
    "Mexico" -> "america-do-norte",
    "Micronesia, Federated States of" -> "",
    "Moldova, Republic of" -> "europa",
    "Monaco" -> "europa",
    "Mongolia" -> "asia",
    "Montenegro" -> "",
    "Montserrat" -> "",
    "Morocco" -> "africa",
    "Mozambique" -> "africa",
    "Myanmar" -> "asia",
    "Namibia" -> "africa",
    "Nauru" -> "",
    "Nepal" -> "asia",
    "Netherlands" -> "europa",
    "New Caledonia" -> "",
    "New Zealand" -> "oceania",
    "Nicaragua" -> "nicaragua",
    "Nigeria" -> "africa",
    "Niger" -> "africa",
    "Niue	" -> "",
    "Norfolk Island" -> "",
    "Northern Mariana Islands" -> "",
    "Norway" -> "europa",
    "Oman" -> "asia",
    "Pakistan" -> "asia",
    "Palau" -> "",
    "Palestine, State of" -> "asia",
    "Panama" -> "panama",
    "Papua New Guinea" -> "",
    "Paraguay" -> "paraguay",
    "Peru" -> "peru",
    "Philippines" -> "asia",
    "Pitcairn" -> "",
    "Poland" -> "europa",
    "Portugal" -> "europa",
    "Puerto Rico" -> "puerto-rico",
    "Qatar" -> "asia",
    "Réunion" -> "",
    "Romania" -> "europa",
    "Russian Federation" -> "europa",
    "Rwanda" -> "africa",
    "Saint Barthélemy" -> "",
    "Saint Helena, Ascension and Tristan da Cunha" -> "",
    "Saint Kitts and Nevis" -> "caribe",
    "Saint Lucia" -> "caribe",
    "Saint Martin (French part)" -> "caribe",
    "Saint Pierre and Miquelon" -> "",
    "Saint Vincent and the Grenadines" -> "",
    "Samoa" -> "",
    "San Marino" -> "europa",
    "Sao Tome and Principe" -> "africa",
    "Saudi Arabia" -> "asia",
    "Senegal" -> "africa",
    "Serbia" -> "europa",
    "Seychelles" -> "",
    "Sierra Leone" -> "africa",
    "Singapore" -> "asia",
    "Sint Maarten (Dutch part)" -> "",
    "Slovakia" -> "europa",
    "Slovenia" -> "europa",
    "Solomon Islands" -> "",
    "Somalia" -> "africa",
    "South Africa" -> "africa",
    "South Georgia and the South Sandwich Islands" -> "caribe",
    "South Sudan" -> "africa",
    "Spain" -> "europa",
    "Sri Lanka" -> "asia",
    "Sudan" -> "africa",
    "Suriname" -> "suriname",
    "Svalbard and Jan Mayen" -> "",
    "Swaziland" -> "",
    "Sweden" -> "europa",
    "Switzerland" -> "europa",
    "Syrian Arab Republic" -> "asia",
    "Taiwan, Province of China" -> "asia",
    "Tajikistan" -> "",
    "Tanzania, United Republic of" -> "",
    "Thailand" -> "asia",
    "Timor-Leste" -> "",
    "Togo" -> "",
    "Tokelau" -> "",
    "Tonga" -> "",
    "Trinidad and Tobago" -> "trinidad-y-tobago",
    "Tunisia" -> "africa",
    "Turkey" -> "europa",
    "Turkmenistan" -> "",
    "Turks and Caicos Islands" -> "",
    "Tuvalu" -> "",
    "Uganda" -> "africa",
    "Ukraine" -> "europa",
    "United Arab Emirates" -> "asia",
    "United Kingdom" -> "europa",
    "United States Minor Outlying Islands" -> "",
    "United States of America" -> "america-do-norte",
    "Uruguay" -> "uruguay",
    "Uzbekistan" -> "asia",
    "Vanuatu" -> "",
    "Venezuela, Bolivarian Republic of" -> "venezuela",
    "Viet Nam" -> "",
    "Virgin Islands, British" -> "caribe",
    "Virgin Islands, U.S." -> "caribe",
    "Wallis and Futuna" -> "",
    "Western Sahara" -> "africa",
    "Yemen" -> "asia",
    "Zambia" -> "africa",
    "Zimbabwe" -> "africa"
  )

  val code2iAHx = Map(
    "AFG" -> "asia",
    "ALB" -> "europa",
    "DZA" -> "",
    "ASM" -> "",
    "AND" -> "europa",
    "AGO" -> "africa",
    "AIA" -> "anguilla",
    "ATA" -> "",
    "ATG" -> "caribe",
    "ARG" -> "argentina",
    "ARM" -> "europa",
    "ABW" -> "caribe",
    "AUS" -> "oceania",
    "AUT" -> "europa",
    "AZE" -> "asia",
    "BHS" -> "caribe",
    "BHR" -> "",
    "BGD" -> "asia",
    "BRB" -> "barbados",
    "BLR" -> "europa",
    "BEL" -> "europa",
    "BLZ" -> "belize",
    "BEN" -> "",
    "BMU" -> "bermudas",
    "BTN" -> "",
    "BOL" -> "bolivia",
    "BES" -> "",
    "BIH" -> "europa",
    "BWA" -> "",
    "BVT" -> "",
    "BRA" -> "brasil",
    "IOT" -> "",
    "BRN" -> "",
    "BGR" -> "europa",
    "BFA" -> "",
    "BDI" -> "",
    "CPV" -> "africa",
    "KHM" -> "asia",
    "CMR" -> "",
    "CAN" -> "america-do-norte",
    "CYM" -> "islas-caiman",
    "CAF" -> "",
    "TCD" -> "",
    "CHL" -> "chile",
    "CHN" -> "asia",
    "CXR" -> "",
    "CCK" -> "",
    "COL" -> "colombia",
    "COM" -> "",
    "COG" -> "africa",
    "COD" -> "africa",
    "COK" -> "",
    "CRI" -> "costa-rica",
    "CIV" -> "",
    "HRV" -> "europa",
    "CUB" -> "cuba",
    "CUW" -> "caribe",
    "CYP" -> "",
    "CZE" -> "europa",
    "DNK" -> "europa",
    "DJI" -> "",
    "DMA" -> "dominica",
    "DOM" -> "republica-dominicana",
    "ECU" -> "ecuador",
    "EGY" -> "africa",
    "SLV" -> "el-salvador",
    "GNQ" -> "africa",
    "ERI" -> "",
    "EST" -> "europa",
    "ETH" -> "africa",
    "FLK" -> "",
    "FRO" -> "europa",
    "FJI" -> "oceania",
    "FIN" -> "europa",
    "FRA" -> "europa",
    "GUF" -> "guyana-francesa",
    "PYF" -> "",
    "ATF" -> "",
    "GAB" -> "",
    "GMB" -> "",
    "GEO" -> "europa",
    "DEU" -> "europa",
    "GHA" -> "africa",
    "GIB" -> "europa",
    "GRC" -> "europa",
    "GRL" -> "america-do-norte",
    "GRD" -> "grenada",
    "GLP" -> "guadalope",
    "GUM" -> "",
    "GTM" -> "guatemala",
    "GGY" -> "",
    "GNB" -> "",
    "GIN" -> "",
    "GUY" -> "guyana",
    "HTI" -> "haiti",
    "HMD" -> "",
    "VAT" -> "",
    "HND" -> "honduras",
    "HKG" -> "asia",
    "HUN" -> "europa",
    "ISL" -> "europa",
    "IND" -> "asia",
    "IDN" -> "asia",
    "IRN" -> "asia",
    "IRQ" -> "asia",
    "IRL" -> "europa",
    "ALA" -> "",
    "IMN" -> "",
    "ISR" -> "asia",
    "ITA" -> "europa",
    "JAM" -> "jamaica",
    "JPN" -> "",
    "JEY" -> "",
    "JOR" -> "asia",
    "KAZ" -> "asia",
    "KEN" -> "africa",
    "KIR" -> "",
    "PRK" -> "asia",
    "KOR" -> "asia",
    "KWT" -> "asia",
    "KGZ" -> "",
    "LAO" -> "",
    "LVA" -> "",
    "LBN" -> "asia",
    "LSO" -> "",
    "LBR" -> "africa",
    "LBY" -> "africa",
    "LIE" -> "europa",
    "LTU" -> "europa",
    "LUX" -> "europa",
    "MAC" -> "europa",
    "MKD" -> "europa",
    "MDG" -> "africa",
    "MWI" -> "",
    "MYS" -> "asia",
    "MDV" -> "",
    "MLI" -> "",
    "MLT" -> "europa",
    "MHL" -> "oceania",
    "MTQ" -> "martinica",
    "MRT" -> "africa",
    "MUS" -> "",
    "MYT" -> "",
    "MEX" -> "america-do-norte",
    "FSM" -> "",
    "MDA" -> "europa",
    "MCO" -> "europa",
    "MNG" -> "asia",
    "MNE" -> "",
    "MSR" -> "",
    "MAR" -> "africa",
    "MOZ" -> "africa",
    "MMR" -> "asia",
    "NAM" -> "africa",
    "NRU" -> "",
    "NPL" -> "asia",
    "NLD" -> "europa",
    "NCL" -> "",
    "NZL" -> "oceania",
    "NIC" -> "nicaragua",
    "NGA" -> "africa",
    "NER" -> "africa",
    "NIU" -> "",
    "NFK" -> "",
    "MNP" -> "",
    "NOR" -> "europa",
    "OMN" -> "asia",
    "PAK" -> "asia",
    "PLW" -> "",
    "PSE" -> "asia",
    "PAN" -> "panama",
    "PNG" -> "",
    "PRY" -> "paraguay",
    "PER" -> "peru",
    "PHL" -> "asia",
    "PCN" -> "",
    "POL" -> "europa",
    "PRT" -> "europa",
    "PRI" -> "puerto-rico",
    "QAT" -> "asia",
    "REU" -> "",
    "ROU" -> "europa",
    "RUS" -> "europa",
    "RWA" -> "africa",
    "BLM" -> "",
    "SHN" -> "",
    "KNA" -> "caribe",
    "LCA" -> "caribe",
    "MAF" -> "caribe",
    "SPM" -> "",
    "VCT" -> "",
    "WSM" -> "",
    "SMR" -> "europa",
    "STP" -> "africa",
    "SAU" -> "asia",
    "SEN" -> "africa",
    "SRB" -> "europa",
    "SYC" -> "",
    "SLE" -> "africa",
    "SGP" -> "asia",
    "SXM" -> "",
    "SVK" -> "europa",
    "SVN" -> "europa",
    "SLB" -> "",
    "SOM" -> "africa",
    "ZAF" -> "africa",
    "SGS" -> "caribe",
    "SSD" -> "africa",
    "ESP" -> "europa",
    "LKA" -> "asia",
    "SDN" -> "africa",
    "SUR" -> "suriname",
    "SJM" -> "",
    "SWZ" -> "",
    "SWE" -> "europa",
    "CHE" -> "europa",
    "SYR" -> "asia",
    "TWN" -> "asia",
    "TJK" -> "",
    "TZA" -> "",
    "THA" -> "asia",
    "TLS" -> "",
    "TGO" -> "",
    "TKL" -> "",
    "TON" -> "",
    "TTO" -> "trinidad-y-tobago",
    "TUN" -> "africa",
    "TUR" -> "europa",
    "TKM" -> "",
    "TCA" -> "",
    "TUV" -> "",
    "UGA" -> "africa",
    "UKR" -> "europa",
    "ARE" -> "asia",
    "GBR" -> "europa",
    "UMI" -> "",
    "USA" -> "america-do-norte",
    "URY" -> "uruguay",
    "UZB" -> "asia",
    "VUT" -> "",
    "VEN" -> "venezuela",
    "VNM" -> "",
    "VGB" -> "caribe",
    "VIR" -> "caribe",
    "WLF" -> "",
    "ESH" -> "africa",
    "YEM" -> "asia",
    "ZMB" -> "africa",
    "ZWE" -> "africa"
  )

  val iahxCode = conceptCode match {
    case Some("CNT") =>
      code match {
        case Some(icode) =>
          code2iAHx.get(icode) match {
            case Some(icode) =>
              if (icode.isEmpty) displayName.flatMap(dn => display2iAHx.get(dn))
              else Some(icode)
            case None => displayName.flatMap(dn => display2iAHx.get(dn))
          }
        case None => displayName.flatMap(dn => display2iAHx.get(dn))
      }
    case _ => None
  }

  override def toSrcExpression(conv: MeshConverter,
                               env: Seq[SearchParameter]): Option[String] = {
//println("===Entrei no toSrcExpression")
    iahxCode.map(ic => s"pais_assunto:(%22${ic}%22)")
  }

  override def getCategories: Seq[Category] = {
    Seq(Category(s"locationOfInterest.addr.${conceptCode.get}",
      code.orElse(displayName).get))
  }

  override def toString =
    s"""locationOfInterest(codeSystem: Option[String] = $codeSystem,
                           conceptCode: Option[String] = $conceptCode,
                           code: Option[String] = $code,
                           displayName: Option[String] = $displayName)"""
}

object LocationOfInterest extends Parser {
  override def parse(parameters: Map[String, String])
    : (Seq[SearchParameter], Map[String, String]) = {
    val (loi, others) =
      parameters.partition(_._1.startsWith("locationOfInterest.addr."))

    Try(
      getLoI(loi, 0, Seq())
    ) match {
      case Success(s) => (s, others)
      case Failure(_) => (Seq(), others)
    }
  }

  private def getLoI(loi: Map[String, String],
                     cardinality: Int,
                     auxSeq: Seq[LocationOfInterest]): Seq[SearchParameter] = {
//println(s"*** getLoI loi=$loi cardinality=$cardinality auxSeq=$auxSeq")

    val cardi = if (cardinality == 0) "" else cardinality.toString
    val (loiCard, other) = loi.partition(
      kv =>
        kv._1 matches
          s"locationOfInterest.addr.(ZIP|CTY|STA|CNT)$cardi")
    if (loiCard.isEmpty) auxSeq
    else {
      val kv = loiCard.head
      val conceptCode = Some(kv._1.split("\\.")(2))
      val (code, displayName) =
        if (kv._2.size == 3) (Some(kv._2), None) else (None, Some(kv._2))

      getLoI(
        other,
        cardinality + 1,
        auxSeq :+ (new LocationOfInterest(conceptCode = conceptCode,
                                          code = code,
                                          displayName = displayName))
      )
    }
  }
}
