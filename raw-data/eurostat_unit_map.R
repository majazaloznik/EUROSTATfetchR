eurostat_unit_map <- tibble::tribble(
  ~eurostat_unit, ~umar_unit,
  "THS", "1000",
  "MIO_EUR", "mio eur",
  "EUR", "eur",
  "PC", "%",
  "PCH", "%",
  "NR", "\u0161tevilo",
  "MEUR_KP", "mio eur",
  "INX", "indeks",
  "I15", "indeks",
  "I20", "indeks",
  "I25", "indeks",
  "I21", "indeks",
  "PC_PNT", "odstotne to\u010dke",
  "THS_HD", "1000",
  "MIO_BAL_VAL", "mio eur",
  "PC_EXP_WRL", "%",
  "MIO_EXP_VAL", "mio eur",
  "FLIGHT", "\u0161tevilo",
  "MIO_EUR_SCA", "mio eur",
  "PCH_Q1_SCA", "%",
  "PCH_Q4_SCA", "%",
  "PC_GDP", "%",
  "TRD_VAL_SCA", "mio eur",
  "PC_ACT", "%",
  "PCH_PRE_PERS_SCA", "%",
  "PCH_SM_PER_SCA", "%",
  "PCH_PRE", "%",
  "PCH_SM", "%",
  "PCH_M1_NSA", "%",
  "PCH_M1_SCA", "%",
  "I21_NSA", "%",
  "BS-ESI-I", "ravnotežje v odstotnih točkah",
  "NAC", "usd"
)


# eurostat_unit_dictionary <- eurostat::get_eurostat_dic("unit")
# eurostat_indic_et_dictionary <- eurostat::get_eurostat_dic("indic_et")
# eurostat_indic_dictionary <- eurostat::get_eurostat_dic("indic")
# eurostat_indic_de_dictionary <- eurostat::get_eurostat_dic("indic_de")
# eurostat_indic_bt_dictionary <- eurostat::get_eurostat_dic("indic_bt")
# eurostat_indic_nrg_dictionary <- eurostat::get_eurostat_dic("indic_nrg")

usethis::use_data(eurostat_unit_map,
                  internal = FALSE,
                  overwrite = TRUE)
