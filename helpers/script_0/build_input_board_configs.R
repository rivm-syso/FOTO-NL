#!/usr/bin/env Rscript

get_board_configs <- function() {
  list(
    aa_en_maas = list(
      board_id = "aa_en_maas",
      datasource = "Aa en Maas",
      output_file = "data bestand_Aa en Maas.xlsx",
      raw_patterns = c(
        "Ws Aa en Maaas/Data vanaf 01011980 tm 31121999.csv",
        "Ws Aa en Maaas/Data vanaf 01012000*.csv"
      ),
      rule_fn = board_rule_aa_en_maas,
      enrich_fn = NULL
    ),
    amstel_gooi_vecht = list(
      board_id = "amstel_gooi_vecht",
      datasource = "Amstel Gooi Vecht",
      output_file = "data bestand_Amstel Gooi Vecht.xlsx",
      raw_patterns = c("WS Amstel Gooi Vecht/*.csv"),
      rule_fn = board_rule_amstel,
      enrich_fn = NULL
    ),
    brabantse_delta = list(
      board_id = "brabantse_delta",
      datasource = "Brabantse Delta",
      output_file = "data bestand_Brabantse Delta.xlsx",
      raw_patterns = c("Ws Brabantse Delta/csv bestanden Brabantse delta/*.csv"),
      rule_fn = board_rule_brabantse_delta,
      enrich_fn = NULL
    ),
    de_dommel = list(
      board_id = "de_dommel",
      datasource = "De Dommel",
      output_file = "data bestand_DeDommel.xlsx",
      raw_patterns = c("Ws De Dommel/Data tox stowa/*.csv"),
      rule_fn = board_rule_de_dommel,
      enrich_fn = NULL
    ),
    delfland = list(
      board_id = "delfland",
      datasource = "Delfland",
      output_file = "data bestand_Delfland.xlsx",
      raw_patterns = c(
        "HH Delfland/1950-2009/Dawacodata Chemie *.xlsx",
        "HH Delfland/2010-2019/Dawacodata Chemie *.xlsx"
      ),
      rule_fn = board_rule_delfland,
      enrich_fn = NULL
    ),
    drents_overijsselse_delta = list(
      board_id = "drents_overijsselse_delta",
      datasource = "Drents Overijsselse Delta",
      output_file = "data bestand_Drents Overijsselse Delta.xlsx",
      raw_patterns = c("Ws Drents Overijsselse Delta/WQM_*.csv"),
      rule_fn = board_rule_drents,
      enrich_fn = NULL
    ),
    hh_van_rijnland = list(
      board_id = "hh_van_rijnland",
      datasource = "HH van Rijnland",
      output_file = "data bestand_HH van Rijnland.xlsx",
      raw_patterns = c("HH Rijnland/Stowa Dump/*.dsv"),
      rule_fn = board_rule_hh_van_rijnland,
      enrich_fn = NULL
    ),
    hollands_noorderkwartier = list(
      board_id = "hollands_noorderkwartier",
      datasource = "Hollands Noorderkwartier",
      output_file = "data bestand_Hollands Noorderkwartier.xlsx",
      raw_patterns = c("HH Hollands Noorderkwartier/hhnk_wrnm.txt"),
      rule_fn = board_rule_hollands_noorderkwartier,
      enrich_fn = enrich_hhnk_entry
    ),
    hollandse_delta = list(
      board_id = "hollandse_delta",
      datasource = "Hollandse Delta",
      output_file = "data bestand_Hollandse Delta.xlsx",
      raw_patterns = c("Ws Hollandse Delta/SharedFiles/KRW export *.csv"),
      rule_fn = board_rule_hollandse_delta,
      enrich_fn = NULL
    ),
    hunze_en_aas = list(
      board_id = "hunze_en_aas",
      datasource = "Hunze en Aas",
      output_file = "data bestand_Hunze en Aas.xlsx",
      raw_patterns = c("Ws Hunze en Aa's/*.csv"),
      rule_fn = board_rule_hunze_en_aas,
      enrich_fn = NULL
    ),
    limburg = list(
      board_id = "limburg",
      datasource = "Limburg",
      output_file = "data bestand_Limburg.xlsx",
      raw_patterns = c(
        "Ws Limburg/OneDrive_3_26-9-2018/WL - f-ch - *.csv",
        "Ws Limburg/WL - f-ch - 2018-m2019 (exp. 2019-12-10).xlsx"
      ),
      rule_fn = board_rule_limburg,
      enrich_fn = NULL
    ),
    noorderzijlvest = list(
      board_id = "noorderzijlvest",
      datasource = "Noorderzijlvest",
      output_file = "data bestand_WS Noorderzijlvest.xlsx",
      raw_patterns = c("Ws Noorderzijlvest/gegevens_nzv_oppwha.xlsx"),
      rule_fn = board_rule_noorderzijlvest,
      enrich_fn = NULL
    ),
    rijn_en_ijssel = list(
      board_id = "rijn_en_ijssel",
      datasource = "Rijn en IJssel",
      output_file = "data bestand_Rijn en IJssel.xlsx",
      raw_patterns = c("Ws Rijn en IJssel/WRIJ_chemie_metingen/allemetingen.csv"),
      rule_fn = board_rule_rijn_en_ijssel,
      enrich_fn = NULL
    ),
    rivierenland = list(
      board_id = "rivierenland",
      datasource = "Rivierenland",
      output_file = "data bestand_Rivierenland.xlsx",
      raw_patterns = c("Ws Rivierenland/*.xlsx"),
      rule_fn = board_rule_rivierenland,
      enrich_fn = NULL
    ),
    scheldestromen = list(
      board_id = "scheldestromen",
      datasource = "Scheldestromen",
      output_file = "data bestand_Scheldestromen.xlsx",
      raw_patterns = c("Ws Scheldestromen/MWA-*.csv"),
      rule_fn = board_rule_scheldestromen,
      enrich_fn = NULL
    ),
    schieland_krimpenerwaard = list(
      board_id = "schieland_krimpenerwaard",
      datasource = "Schieland Krimpenerwaard",
      output_file = "data bestand_Schieland Krimpenerwaard.xlsx",
      raw_patterns = c("HH Schieland&Krimpenerwaard/Fysisch_chemische_meetgegevens_HHSK.csv"),
      rule_fn = board_rule_schieland,
      enrich_fn = NULL
    ),
    stichtse_rijnlanden = list(
      board_id = "stichtse_rijnlanden",
      datasource = "Stichtse Rijnlanden",
      output_file = "data bestand_Stichtse Rijnlanden.xlsx",
      raw_patterns = c("HH Stichtse Rijnlanden/HDSR_*.csv"),
      rule_fn = board_rule_stichtse_rijnlanden,
      enrich_fn = NULL
    ),
    vallei_veluwe = list(
      board_id = "vallei_veluwe",
      datasource = "Vallei Veluwe",
      output_file = "data bestand_Vallei Veluwe.xlsx",
      raw_patterns = c(
        "Ws Vallei en Veluwe/alle data *.csv",
        "Ws Vallei en Veluwe/alle data 2018.xlsx"
      ),
      rule_fn = board_rule_vallei_veluwe,
      enrich_fn = NULL
    ),
    vechtstromen = list(
      board_id = "vechtstromen",
      datasource = "Vechtstromen",
      output_file = "data bestand_Vechtstromen_aangepast 19 aug 2020.xlsx",
      raw_patterns = c("Ws Vechtstromen/*.csv"),
      rule_fn = board_rule_vechtstromen,
      enrich_fn = NULL
    ),
    wetterskip = list(
      board_id = "wetterskip",
      datasource = "Wetterskip",
      output_file = "data bestand_Wetterskip.xlsx",
      raw_patterns = c("Wetterskip/WF[1-4].txt"),
      rule_fn = board_rule_wetterskip,
      enrich_fn = NULL
    ),
    zuiderzeeland = list(
      board_id = "zuiderzeeland",
      datasource = "Zuiderzeeland",
      output_file = "data bestand_Zuiderzeeland.xlsx",
      raw_patterns = c("Ws Zuiderzeeland/zzl_export*.csv"),
      rule_fn = board_rule_zuiderzeeland,
      enrich_fn = NULL
    )
  )
}

get_board_config <- function(board_id) {
  configs <- get_board_configs()
  if (!(board_id %in% names(configs))) {
    stop("Unknown board_id: ", board_id)
  }
  configs[[board_id]]
}

list_board_ids <- function() {
  names(get_board_configs())
}
