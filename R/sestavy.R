load_efs_fin <- function(dir, filename) {

  path <- file.path(dir, filename)

  efs_fin_headers <- read_excel(path, skip = 1, n_max = 2)
  efs_fin_headers_l <- efs_fin_headers %>% t() %>%
    as.data.frame() %>%
    fill(V1, .direction = "down")

  efs_fin_headers_l %>% distinct(V2)

  translate_v1 <- tribble(
    ~V1, ~one,
    "projekt", "prj",
    "stav projektu", "real_stav",
    "poměry zdrojů financování", "pomer",
    "finanční prostředky v zaregistrovaných žádostech o podporu (číslo 13)", "fin_zadost",
    "finanční prostředky v právních aktech o poskytnutí / převodu podpory (číslo 39)", "fin_pravniakt",
    "finanční prostředky vyúčtované v žádostech o platbu (číslo 50)", "fin_vyuct",
    "finanční prostředky ve finančně ukončených operacích ze strany MF-PCO (číslo 65)", "fin_ukonc"
  )

  translate_v2 <- tribble(
    ~V2, ~two,
    "číslo projektu", "id",
    "název projektu", "název",
    "číslo stavu", "id",
    "název stavu", "název",
    "příspěvek Unie", "eu",
    "národní zdroje", "národní",
    "vlastní podíl", "vlastní",
    "celkové zdroje připadající na způsobilé výdaje", "czv",
    "finanční prostředky ze státního rozpočtu", "sr",
    "finanční prostředky ze státních fondů", "sf",
    "finanční prostředky z rozpočtu krajů/kraje", "kraj",
    "finanční prostředky z rozpočtu obcí/obce", "obec",
    "jiné národní veřejné finanční prostředky", "jine_nar_ver",
    "soukromé zdroje", "soukr")

  efs_fin_names <- efs_fin_headers_l %>%
    left_join(translate_v1) %>%
    left_join(translate_v2) %>%
    unite(col = "name", one, two) %>%
    pull(name)

  efs_fin <- read_excel("data-input/sestavy/E003 Finance celkem 20210128.xlsx",
                        col_names = efs_fin_names, skip = 4) %>%
    janitor::clean_names() %>%
    select(-prj_nazev)

  return(efs_fin)
}

load_efs_zop <- function(dir, filename) {

  path <- file.path(dir, filename)

  efs_zop0 <- read_excel(path, skip = 2) %>%
    janitor::clean_names() %>%
    select(-nazev_projektu)

  efs_zop <- efs_zop0 %>%
    rename(
      prj_id = cislo_projektu,
      real_stav_kod = cislo_stavu,
      dt_zop_proplaceni = datum_proplaceni,
      real_stav_nazev = nazev_stavu,
      zop_cislo = cislo_zo_p,
      pomer_unie = prispevek_unie_7,
      pomer_narodni = narodni_zdroje,
      pomer_vlastni = vlastni_podil,
      fin_vyuct_czv = celkove_zdroje_pripadajici_na_zpusobile_vydaje,
      fin_vyuct_eu = prispevek_unie_11,
      fin_vyuct_sr = financni_prostredky_ze_statniho_rozpoctu,
      fin_vyuct_sf = financni_prostredky_ze_statnich_fondu,
      fin_vyuct_kraj = financni_prostredky_z_rozpoctu_kraju_kraje,
      fin_vyuct_obec = financni_prostredky_z_rozpoctu_obci_obce,
      fin_vyuct_jine_narodni_verejne = jine_narodni_verejne_financni_prostredky,
      fin_vyuct_soukrome = soukrome_zdroje
    )

  return(efs_zop)
}


load_efs_prj <- function(dir, filename) {

  path <- file.path(dir, filename)

  efs_prj0 <- read_excel(path, skip = 3) %>%
    janitor::clean_names() %>%
    select(-ends_with("_in"))

  efs_prj <- efs_prj0 %>%
    rename(prj_id = cislo_projektu,
           prj_nazev = nazev_projektu,
           real_stav_kod = cislo_stavu,
           real_stav_nazev = nazev_stavu,
           op_id = cislo_programu,
           op_nazev = nazev_programu,
           po_id = cislo_prioritni_osy,
           po_nazev = nazev_prioritni_osy,
           sc_id = cislo_sc,
           sc_nazev = nazev_sc,
           sc_podil = percent_sc,
           vyzva_id = cislo_vyzvy,
           vyzva_nazev = nazev_vyzvy,
           kat_kod = kod,
           kat_nazev = nazev,
           dt_zadost = prvniho_prechodu_do_stavu_pp20,
           dt_smlouva = podpisu_smlouvy,
           dt_ukon_fyz = fyzickeho_ukonceni,
           dt_ukon_cert = financniho_ukonceni_projektu_certifikace_pp42,
           dt_ukon_uzavr = finalniho_uzavreni_projektu_pp43,
           dt_ukon_fyz_predpokl = predpokladane_datum_ukonceni_fyzicke_realizace_projektu_21,
           dt_zahaj_fyz_predpokl = predpokladane_datum_ukonceni_fyzicke_realizace_projektu_20
    ) %>%
    drop_na(prj_id)
    drop_na(prj_id) %>%
    add_op_labels()
  return(efs_prj)
}

load_efs_obl <- function(dir, filename) {
  path <- file.path(dir, filename)

  efs_obl0 <- read_excel(path, skip = 3, col_types = c("text", rep("text", 3),
                                                       "text", "text",
                                                       "numeric", "numeric")) %>%
    janitor::clean_names()

  efs_obl <- efs_obl0 %>%
    rename(prj_id = registracni_cislo_projektu,
           oblast_intervence_podil = oblast_intervence_procentni_podil,
           fin_eu = fin_prostredky_eu)

  return(efs_obl)
}

# efs_zop %>%
#   count(wt = `celkové zdroje připadající na způsobilé výdaje`)
#
# efs_fin %>%
#   count(wt = vyuct_czv)
#
# efs_pub %>%
#   ungroup() %>%
#   count(wt = vyuct_v_zop_celkove_zpusobile_vydaje_czk)
#
# efs_zop %>%
#   count(real_stav_nazev, wt = vyuct_czv/1e9)
