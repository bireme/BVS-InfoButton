/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

object ISO3166 {
  val codes1 = Map(
    "Afghanistan" -> "asia",
    "Albania" -> "europa",
    "Algeria" -> "africa",
    "American Samoa" -> "oceania",
    "Andorra" -> "europa",
    "Angola" -> "africa",
    "Anguilla" -> "anguilla",
    "Antarctica" -> "antartida",
    "Antigua and Barbuda" -> "caribe",
    "Argentina" -> "argentina",
    "Armenia" -> "europa",
    "Aruba" -> "caribe",
    "Australia" -> "oceania",
    "Austria" -> "europa",
    "Azerbaijan" -> "asia",
    "Bahamas" -> "caribe",
    "Bahrain" -> "asia",
    "Bangladesh" -> "asia",
    "Barbados" -> "barbados",
    "Belarus" -> "europa",
    "Belgium" -> "europa",
    "Belize" -> "belize",
    "Benin" -> "africa",
    "Bermuda" -> "bermudas",
    "Bhutan" -> "asia",
    "Bolivia, Plurinational State of" -> "bolivia",
    "Bonaire, Sint Eustatius and Saba" -> "america-do-sul",
    "Bosnia and Herzegovina" -> "europa",
    "Botswana" -> "africa",
    "Bouvet Island" -> "",  // ilha
    "Brazil" -> "brasil",
    "British Indian Ocean Territory" -> "asia",
    "Brunei Darussalam" -> "asia",
    "Bulgaria" -> "europa",
    "Burkina Faso" -> "africa",
    "Burundi" -> "africa",
    "Cabo Verde" -> "africa",
    "Cambodia" -> "asia",
    "Cameroon" -> "africa",
    "Canada" -> "america-do-norte",
    "Cayman Islands" -> "islas-caiman",
    "Central African Republic" -> "africa",
    "Chad" -> "africa",
    "Chile" -> "chile",
    "China" -> "asia",
    "Christmas Island" -> "asia",
    "Cocos (Keeling) Islands" -> "asia",
    "Colombia" -> "colombia",
    "Comoros" -> "africa",
    "Congo" -> "africa",
    "Congo, the Democratic Republic of the" -> "africa",
    "Cook Islands" -> "oceania",
    "Costa Rica" -> "costa-rica",
    "Côte d'Ivoire" -> "africa",
    "Croatia" -> "europa",
    "Cuba" -> "cuba",
    "Curaçao" -> "caribe",
    "Cyprus" -> "europa",
    "Czechia" -> "europa",
    "Denmark" -> "europa",
    "Djibouti" -> "africa",
    "Dominica" -> "dominica",
    "Dominican Republic" -> "republica-dominicana",
    "Ecuador" -> "ecuador",
    "Egypt" -> "africa",
    "El Salvador" -> "el-salvador",
    "Equatorial Guinea" -> "africa",
    "Eritrea" -> "africa",
    "Estonia" -> "europa",
    "Ethiopia" -> "africa",
    "Falkland Islands (Malvinas)" -> "america-do-sul",
    "Faroe Islands" -> "europa",
    "Fiji" -> "oceania",
    "Finland" -> "europa",
    "France" -> "europa",
    "French Guiana" -> "guyana-francesa",
    "French Polynesia" -> "oceania",
    "French Southern Territories" -> "", // ???
    "Gabon" -> "africa",
    "Gambia" -> "africa",
    "Georgia" -> "europa",
    "Germany" -> "europa",
    "Ghana" -> "africa",
    "Gibraltar" -> "europa",
    "Greece" -> "europa",
    "Greenland" -> "america-do-norte",
    "Grenada" -> "grenada",
    "Guadeloupe" -> "guadalope",
    "Guam" -> "oceania",
    "Guatemala" -> "guatemala",
    "Guernsey" -> "europa",
    "Guinea-Bissau" -> "africa",
    "Guinea" -> "africa",
    "Guyana" -> "guyana",
    "Haiti" -> "haiti",
    "Heard Island and McDonald Islands" -> "", // ilhas
    "Holy See" -> "europa",
    "Honduras" -> "honduras",
    "Hong Kong" -> "asia",
    "Hungary" -> "europa",
    "Iceland" -> "europa",
    "India" -> "asia",
    "Indonesia" -> "asia",
    "Iran, Islamic Republic of" -> "asia",
    "Iraq" -> "asia",
    "Ireland" -> "europa",
    "Islands" -> "", // ???
    "Isle of Man" -> "europa",
    "Israel" -> "asia",
    "Italy" -> "europa",
    "Jamaica" -> "jamaica",
    "Japan" -> "asia",
    "Jersey" -> "europa",
    "Jordan" -> "asia",
    "Kazakhstan" -> "asia",
    "Kenya" -> "africa",
    "Kiribati" -> "oceania",
    "Korea, Democratic People's Republic of" -> "asia",
    "Korea, Republic of" -> "asia",
    "Kuwait" -> "asia",
    "Kyrgyzstan" -> "asia",
    "Lao People's Democratic Republic" -> "asia",
    "Latvia" -> "europa",
    "Lebanon" -> "asia",
    "Lesotho" -> "africa",
    "Liberia" -> "africa",
    "Libya" -> "africa",
    "Liechtenstein" -> "europa",
    "Lithuania" -> "europa",
    "Luxembourg" -> "europa",
    "Macao" -> "europa",
    "Macedonia, the former Yugoslav Republic of" -> "europa",
    "Madagascar" -> "africa",
    "Malawi" -> "africa",
    "Malaysia" -> "asia",
    "Maldives" -> "asia",
    "Mali" -> "africa",
    "Malta" -> "europa",
    "Marshall Islands" -> "oceania",
    "Martinique" -> "martinica",
    "Mauritania" -> "africa",
    "Mauritius" -> "africa",
    "Mayotte" -> "", // ???
    "Mexico" -> "america-do-norte",
    "Micronesia, Federated States of" -> "oceania",
    "Moldova, Republic of" -> "europa",
    "Monaco" -> "europa",
    "Mongolia" -> "asia",
    "Montenegro" -> "europa",
    "Montserrat" -> "",  // ???
    "Morocco" -> "africa",
    "Mozambique" -> "africa",
    "Myanmar" -> "asia",
    "Namibia" -> "africa",
    "Nauru" -> "oceania",
    "Nepal" -> "asia",
    "Netherlands" -> "europa",
    "New Caledonia" -> "oceania",
    "New Zealand" -> "oceania",
    "Nicaragua" -> "nicaragua",
    "Nigeria" -> "africa",
    "Niger" -> "africa",
    "Niue" -> "oceania",
    "Norfolk Island" -> "oceania",
    "Northern Mariana Islands" -> "oceania",
    "Norway" -> "europa",
    "Oman" -> "asia",
    "Pakistan" -> "asia",
    "Palau" -> "oceania",
    "Palestine, State of" -> "asia",
    "Panama" -> "panama",
    "Papua New Guinea" -> "oceania",
    "Paraguay" -> "paraguay",
    "Peru" -> "peru",
    "Philippines" -> "asia",
    "Pitcairn" -> "oceania",
    "Poland" -> "europa",
    "Portugal" -> "europa",
    "Puerto Rico" -> "puerto-rico",
    "Qatar" -> "asia",
    "Réunion" -> "africa",
    "Romania" -> "europa",
    "Russian Federation" -> "europa",
    "Rwanda" -> "africa",
    "Saint Barthélemy" -> "america-do-norte",
    "Saint Helena, Ascension and Tristan da Cunha" -> "africa",
    "Saint Kitts and Nevis" -> "caribe",
    "Saint Lucia" -> "caribe",
    "Saint Martin (French part)" -> "caribe",
    "Saint Pierre and Miquelon" -> "america-do-norte",
    "Saint Vincent and the Grenadines" -> "america-do-norte",
    "Samoa" -> "oceania",
    "San Marino" -> "europa",
    "Sao Tome and Principe" -> "africa",
    "Saudi Arabia" -> "asia",
    "Senegal" -> "africa",
    "Serbia" -> "europa",
    "Seychelles" -> "africa",
    "Sierra Leone" -> "africa",
    "Singapore" -> "asia",
    "Sint Maarten (Dutch part)" -> "america-do-norte",
    "Slovakia" -> "europa",
    "Slovenia" -> "europa",
    "Solomon Islands" -> "oceania",
    "Somalia" -> "africa",
    "South Africa" -> "africa",
    "South Georgia and the South Sandwich Islands" -> "caribe",
    "South Sudan" -> "africa",
    "Spain" -> "europa",
    "Sri Lanka" -> "asia",
    "Sudan" -> "africa",
    "Suriname" -> "suriname",
    "Svalbard and Jan Mayen" -> "europa",
    "Swaziland" -> "africa",
    "Sweden" -> "europa",
    "Switzerland" -> "europa",
    "Syrian Arab Republic" -> "asia",
    "Taiwan, Province of China" -> "asia",
    "Tajikistan" -> "asia",
    "Tanzania, United Republic of" -> "africa",
    "Thailand" -> "asia",
    "Timor-Leste" -> "asia",
    "Togo" -> "africa",
    "Tokelau" -> "oceania",
    "Tonga" -> "oceania",
    "Trinidad and Tobago" -> "trinidad-y-tobago",
    "Tunisia" -> "africa",
    "Turkey" -> "europa",
    "Turkmenistan" -> "asia",
    "Turks and Caicos Islands" -> "america-do-norte",
    "Tuvalu" -> "oceania",
    "Uganda" -> "africa",
    "Ukraine" -> "europa",
    "United Arab Emirates" -> "asia",
    "United Kingdom" -> "europa",
    "United States Minor Outlying Islands" -> "america-do-norte",
    "United States of America" -> "america-do-norte",
    "Uruguay" -> "uruguay",
    "Uzbekistan" -> "asia",
    "Vanuatu" -> "oceania",
    "Venezuela, Bolivarian Republic of" -> "venezuela",
    "Viet Nam" -> "asia",
    "Virgin Islands, British" -> "caribe",
    "Virgin Islands, U.S." -> "caribe",
    "Wallis and Futuna" -> "oceania",
    "Western Sahara" -> "africa",
    "Yemen" -> "asia",
    "Zambia" -> "africa",
    "Zimbabwe" -> "africa"
  )

  val codes2 =   Map(
    "AFG" -> "asia",
    "ALB" -> "europa",
    "DZA" -> "africa",
    "ASM" -> "oceania",
    "AND" -> "europa",
    "AGO" -> "africa",
    "AIA" -> "anguilla",
    "ATA" -> "antartida",
    "ATG" -> "caribe",
    "ARG" -> "argentina",
    "ARM" -> "europa",
    "ABW" -> "caribe",
    "AUS" -> "oceania",
    "AUT" -> "europa",
    "AZE" -> "asia",
    "BHS" -> "caribe",
    "BHR" -> "asia",
    "BGD" -> "asia",
    "BRB" -> "barbados",
    "BLR" -> "europa",
    "BEL" -> "europa",
    "BLZ" -> "belize",
    "BEN" -> "africa",
    "BMU" -> "bermudas",
    "BTN" -> "asia",
    "BOL" -> "bolivia",
    "BES" -> "america-do-sul",
    "BIH" -> "europa",
    "BWA" -> "africa",
    "BVT" -> "",
    "BRA" -> "brasil",
    "IOT" -> "asia",
    "BRN" -> "asia",
    "BGR" -> "europa",
    "BFA" -> "africa",
    "BDI" -> "africa",
    "CPV" -> "africa",
    "KHM" -> "asia",
    "CMR" -> "africa",
    "CAN" -> "america-do-norte",
    "CYM" -> "islas-caiman",
    "CAF" -> "africa",
    "TCD" -> "africa",
    "CHL" -> "chile",
    "CHN" -> "asia",
    "CXR" -> "asia",
    "CCK" -> "asia",
    "COL" -> "colombia",
    "COM" -> "africa",
    "COG" -> "africa",
    "COD" -> "africa",
    "COK" -> "oceania",
    "CRI" -> "costa-rica",
    "CIV" -> "africa",
    "HRV" -> "europa",
    "CUB" -> "cuba",
    "CUW" -> "caribe",
    "CYP" -> "europa",
    "CZE" -> "europa",
    "DNK" -> "europa",
    "DJI" -> "africa",
    "DMA" -> "dominica",
    "DOM" -> "republica-dominicana",
    "ECU" -> "ecuador",
    "EGY" -> "africa",
    "SLV" -> "el-salvador",
    "GNQ" -> "africa",
    "ERI" -> "africa",
    "EST" -> "europa",
    "ETH" -> "africa",
    "FLK" -> "america-do-sul",
    "FRO" -> "europa",
    "FJI" -> "oceania",
    "FIN" -> "europa",
    "FRA" -> "europa",
    "GUF" -> "guyana-francesa",
    "PYF" -> "oceania",
    "ATF" -> "",
    "GAB" -> "africa",
    "GMB" -> "africa",
    "GEO" -> "europa",
    "DEU" -> "europa",
    "GHA" -> "africa",
    "GIB" -> "europa",
    "GRC" -> "europa",
    "GRL" -> "america-do-norte",
    "GRD" -> "grenada",
    "GLP" -> "guadalope",
    "GUM" -> "oceania",
    "GTM" -> "guatemala",
    "GGY" -> "europa",
    "GNB" -> "africa",
    "GIN" -> "africa",
    "GUY" -> "guyana",
    "HTI" -> "haiti",
    "HMD" -> "",
    "VAT" -> "europa",
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
    "IMN" -> "europa",
    "ISR" -> "asia",
    "ITA" -> "europa",
    "JAM" -> "jamaica",
    "JPN" -> "asia",
    "JEY" -> "europa",
    "JOR" -> "asia",
    "KAZ" -> "asia",
    "KEN" -> "africa",
    "KIR" -> "oceania",
    "PRK" -> "asia",
    "KOR" -> "asia",
    "KWT" -> "asia",
    "KGZ" -> "asia",
    "LAO" -> "asia",
    "LVA" -> "europa",
    "LBN" -> "asia",
    "LSO" -> "africa",
    "LBR" -> "africa",
    "LBY" -> "africa",
    "LIE" -> "europa",
    "LTU" -> "europa",
    "LUX" -> "europa",
    "MAC" -> "europa",
    "MKD" -> "europa",
    "MDG" -> "africa",
    "MWI" -> "africa",
    "MYS" -> "asia",
    "MDV" -> "asia",
    "MLI" -> "africa",
    "MLT" -> "europa",
    "MHL" -> "oceania",
    "MTQ" -> "martinica",
    "MRT" -> "africa",
    "MUS" -> "africa",
    "MYT" -> "",
    "MEX" -> "america-do-norte",
    "FSM" -> "oceania",
    "MDA" -> "europa",
    "MCO" -> "europa",
    "MNG" -> "asia",
    "MNE" -> "europa",
    "MSR" -> "",
    "MAR" -> "africa",
    "MOZ" -> "africa",
    "MMR" -> "asia",
    "NAM" -> "africa",
    "NRU" -> "oceania",
    "NPL" -> "asia",
    "NLD" -> "europa",
    "NCL" -> "oceania",
    "NZL" -> "oceania",
    "NIC" -> "nicaragua",
    "NGA" -> "africa",
    "NER" -> "africa",
    "NIU" -> "oceania",
    "NFK" -> "oceania",
    "MNP" -> "oceania",
    "NOR" -> "europa",
    "OMN" -> "asia",
    "PAK" -> "asia",
    "PLW" -> "oceania",
    "PSE" -> "asia",
    "PAN" -> "panama",
    "PNG" -> "oceania",
    "PRY" -> "paraguay",
    "PER" -> "peru",
    "PHL" -> "asia",
    "PCN" -> "oceania",
    "POL" -> "europa",
    "PRT" -> "europa",
    "PRI" -> "puerto-rico",
    "QAT" -> "asia",
    "REU" -> "africa",
    "ROU" -> "europa",
    "RUS" -> "europa",
    "RWA" -> "africa",
    "BLM" -> "america-do-norte",
    "SHN" -> "africa",
    "KNA" -> "caribe",
    "LCA" -> "caribe",
    "MAF" -> "caribe",
    "SPM" -> "america-do-norte",
    "VCT" -> "america-do-norte",
    "WSM" -> "oceania",
    "SMR" -> "europa",
    "STP" -> "africa",
    "SAU" -> "asia",
    "SEN" -> "africa",
    "SRB" -> "europa",
    "SYC" -> "africa",
    "SLE" -> "africa",
    "SGP" -> "asia",
    "SXM" -> "america-do-norte",
    "SVK" -> "europa",
    "SVN" -> "europa",
    "SLB" -> "oceania",
    "SOM" -> "africa",
    "ZAF" -> "africa",
    "SGS" -> "caribe",
    "SSD" -> "africa",
    "ESP" -> "europa",
    "LKA" -> "asia",
    "SDN" -> "africa",
    "SUR" -> "suriname",
    "SJM" -> "europa",
    "SWZ" -> "africa",
    "SWE" -> "europa",
    "CHE" -> "europa",
    "SYR" -> "asia",
    "TWN" -> "asia",
    "TJK" -> "asia",
    "TZA" -> "africa",
    "THA" -> "asia",
    "TLS" -> "asia",
    "TGO" -> "africa",
    "TKL" -> "oceania",
    "TON" -> "oceania",
    "TTO" -> "trinidad-y-tobago",
    "TUN" -> "africa",
    "TUR" -> "europa",
    "TKM" -> "asia",
    "TCA" -> "america-do-norte",
    "TUV" -> "oceania",
    "UGA" -> "africa",
    "UKR" -> "europa",
    "ARE" -> "asia",
    "GBR" -> "europa",
    "UMI" -> "america-do-norte",
    "USA" -> "america-do-norte",
    "URY" -> "uruguay",
    "UZB" -> "asia",
    "VUT" -> "oceania",
    "VEN" -> "venezuela",
    "VNM" -> "asia",
    "VGB" -> "caribe",
    "VIR" -> "caribe",
    "WLF" -> "oceania",
    "ESH" -> "africa",
    "YEM" -> "asia",
    "ZMB" -> "africa",
    "ZWE" -> "africa"
  )
}
