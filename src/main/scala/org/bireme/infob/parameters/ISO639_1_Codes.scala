/*=========================================================================

    BVS-InfoButton © Pan American Health Organization, 2017.
    See License at: https://github.com/bireme/BVS-InfoButton/blob/master/LICENSE.txt

  ==========================================================================*/

package org.bireme.infob.parameters

// https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
object ISO639_1_Codes {
  val codes = Map(
    "ab" -> List("Abkhazian"),
    "aa" -> List("Afar"),
    "af" -> List("Afrikaans"),
    "ak" -> List("Akan"),
    "sq" -> List("Albanian"),
    "am" -> List("Amharic"),
    "ar" -> List("Arabic"),
    "an" -> List("Aragonese"),
    "hy" -> List("Armenian"),
    "as" -> List("Assamese"),
    "av" -> List("Avaric"),
    "ae" -> List("Avestan"),
    "ay" -> List("Aymara"),
    "az" -> List("Azerbaijani"),
    "bm" -> List("Bambara"),
    "ba" -> List("Bashkir"),
    "eu" -> List("Basque"),
    "be" -> List("Belarusian"),
    "bn" -> List("Bengali"),
    "bh" -> List("Bihari languages"),
    "bi" -> List("Bislama"),
    "bs" -> List("Bosnian"),
    "br" -> List("Breton"),
    "bg" -> List("Bulgarian"),
    "my" -> List("Burmese"),
    "ca" -> List("Catalan", "Valencian"),
    "ch" -> List("Chamorro"),
    "ce" -> List("Chechen"),
    "ny" -> List("Chichewa", "Chewa", "Nyanja"),
    "zh" -> List("Chinese"),
    "cv" -> List("Chuvash"),
    "kw" -> List("Cornish"),
    "co" -> List("Corsican"),
    "cr" -> List("Cree"),
    "hr" -> List("Croatian"),
    "cs" -> List("Czech"),
    "da" -> List("Danish"),
    "dv" -> List("Divehi", "Dhivehi", "Maldivian"),
    "nl" -> List("Dutch", "Flemish"),
    "dz" -> List("Dzongkha"),
    "en" -> List("English"),
    "eo" -> List("Esperanto"),
    "et" -> List("Estonian"),
    "ee" -> List("Ewe"),
    "fo" -> List("Faroese"),
    "fj" -> List("Fijian"),
    "fi" -> List("Finnish"),
    "fr" -> List("French"),
    "ff" -> List("Fulah"),
    "gl" -> List("Galician"),
    "ka" -> List("Georgian"),
    "de" -> List("German"),
    "el" -> List("Greek (modern)"),
    "gn" -> List("Guaraní"),
    "gu" -> List("Gujarati"),
    "ht" -> List("Haitian", "Haitian Creole"),
    "ha" -> List("Hausa ha"),
    "he" -> List("Hebrew (modern)"),
    "hz" -> List("Herero"),
    "hi" -> List("Hindi"),
    "ho" -> List("Hiri Motu"),
    "hu" -> List("Hungarian"),
    "ia" -> List("Interlingua"),
    "id" -> List("Indonesian"),
    "ie" -> List("Interlingue"),
    "ga" -> List("Irish"),
    "ig" -> List("Igbo"),
    "ik" -> List("Inupiaq"),
    "io" -> List("Ido"),
    "is" -> List("Icelandic"),
    "it" -> List("Italian"),
    "iu" -> List("Inuktitut"),
    "ja" -> List("Japanese"),
    "jv" -> List("Javanese"),
    "kl" -> List("Kalaallisut", "Greenlandic"),
    "kn" -> List("Kannada"),
    "kr" -> List("Kanuri"),
    "ks" -> List("Kashmiri"),
    "kk" -> List("Kazakh"),
    "km" -> List("Central Khmer"),
    "ki" -> List("Kikuyu", "Gikuyu"),
    "rw" -> List("Kinyarwanda"),
    "ky" -> List("Kirghiz", "Kyrgyz"),
    "kv" -> List("Komi"),
    "kg" -> List("Kongo"),
    "ko" -> List("Korean"),
    "ku" -> List("Kurdish"),
    "kj" -> List("Kuanyama", "Kwanyama"),
    "la" -> List("Latin"),
    "lb" -> List("Luxembourgish", "Letzeburgesch"),
    "lg" -> List("Ganda"),
    "li" -> List("Limburgan", "Limburger", "Limburgish"),
    "ln" -> List("Lingala"),
    "lo" -> List("Lao"),
    "lt" -> List("Lithuanian"),
    "lu" -> List("Luba-Katanga"),
    "lv" -> List("Latvian"),
    "gv" -> List("Manx"),
    "mk" -> List("Macedonian"),
    "mg" -> List("Malagasy"),
    "ms" -> List("Malay"),
    "ml" -> List("Malayalam"),
    "mt" -> List("Maltese"),
    "mi" -> List("Maori"),
    "mr" -> List("Marathi"),
    "mh" -> List("Marshallese"),
    "mn" -> List("Mongolian"),
    "na" -> List("Nauru"),
    "nv" -> List("Navajo", "Navaho"),
    "nd" -> List("North Ndebele"),
    "ne" -> List("Nepali"),
    "ng" -> List("Ndonga"),
    "nb" -> List("Norwegian Bokmål"),
    "nn" -> List("Norwegian Nynorsk"),
    "no" -> List("Norwegian"),
    "ii" -> List("Sichuan Yi", "Nuosu"),
    "nr" -> List("South Ndebele"),
    "oc" -> List("Occitan"),
    "oj" -> List("Ojibwa"),
    "cu" -> List("Church Slavic", "Church Slavonic", "Old Church Slavonic", "Old Slavonic", "Old Bulgarian"),
    "om" -> List("Oromo"),
    "or" -> List("Oriya"),
    "os" -> List("Ossetian", "Ossetic"),
    "pa" -> List("Panjabi", "Punjabi"),
    "pi" -> List("Pali"),
    "fa" -> List("Persian"),
    "pox" -> List("Polabian"),
    "pl" -> List("Polish"),
    "ps" -> List("Pashto", "Pushto"),
    "pt" -> List("Portuguese"),
    "qu" -> List("Quechua"),
    "rm" -> List("Romansh"),
    "rn" -> List("Rundi"),
    "ro" -> List("Romanian", "Moldavian", "Moldovan"),
    "ru" -> List("Russian"),
    "sa" -> List("Sanskrit"),
    "sc" -> List("Sardinian"),
    "sd" -> List("Sindhi"),
    "se" -> List("Northern Sami"),
    "sm" -> List("Samoan"),
    "sg" -> List("Sango"),
    "sr" -> List("Serbian"),
    "gd" -> List("Gaelic", "Scottish Gaelic"),
    "sn" -> List("Shona"),
    "si" -> List("Sinhala", "Sinhalese"),
    "sk" -> List("Slovak"),
    "sl" -> List("Slovenian"),
    "so" -> List("Somali"),
    "st" -> List("Southern Sotho"),
    "es" -> List("Spanish", "Castilian"),
    "su" -> List("Sundanese"),
    "sw" -> List("Swahili"),
    "ss" -> List("Swati"),
    "sv" -> List("Swedish"),
    "ta" -> List("Tamil"),
    "te" -> List("Telugu"),
    "tg" -> List("Tajik"),
    "th" -> List("Thai"),
    "ti" -> List("Tigrinya"),
    "bo" -> List("Tibetan"),
    "tk" -> List("Turkmen"),
    "tl" -> List("Tagalog"),
    "tn" -> List("Tswana"),
    "to" -> List("Tonga (Tonga Islands)"),
    "tr" -> List("Turkish"),
    "ts" -> List("Tsonga"),
    "tt" -> List("Tatar"),
    "tw" -> List("Twi"),
    "ty" -> List("Tahitian"),
    "ug" -> List("Uighur", "Uyghur"),
    "uk" -> List("Ukrainian"),
    "ur" -> List("Urdu"),
    "uz" -> List("Uzbek"),
    "ve" -> List("Venda"),
    "vi" -> List("Vietnamese"),
    "vo" -> List("Volapük"),
    "wa" -> List("Walloon"),
    "cy" -> List("Welsh"),
    "wo" -> List("Wolof"),
    "fy" -> List("Western Frisian"),
    "xh" -> List("Xhosa"),
    "yi" -> List("Yiddish"),
    "yo" -> List("Yoruba"),
    "za" -> List("Zhuang", "Chuang"),
    "zu" -> List("Zulu")
  )
}
