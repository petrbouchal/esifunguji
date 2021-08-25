load_efs_fin <- function(dir, filename) {

  path <- file.path(dir, filename)

  efs_fin_headers <- read_excel(path, skip = 0, n_max = 2,
                                col_names = FALSE)
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

  efs_fin <- read_excel(path,
                        col_names = efs_fin_names, skip = 4) %>%
    janitor::clean_names() %>%
    select(-prj_nazev)

  return(efs_fin)
}

load_efs_zop <- function(dir, filename) {

  path <- file.path(dir, filename)

  efs_zop0 <- read_excel(path, skip = 0, col_types = "guess", guess_max = 10000) %>%
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
      fin_vyuct_soukr = soukrome_zdroje,
      fin_vyuct_sr = financni_prostredky_ze_statniho_rozpoctu,
      fin_vyuct_sf = financni_prostredky_ze_statnich_fondu,
      fin_vyuct_kraj = financni_prostredky_z_rozpoctu_kraju_kraje,
      fin_vyuct_obec = financni_prostredky_z_rozpoctu_obci_obce,
      fin_vyuct_jine_nar_ver = jine_narodni_verejne_financni_prostredky
    ) %>%
    mutate(
      across(starts_with("fin_"), as.double),
      across(starts_with("pomer_"), as.double),
      across(starts_with("dt_"), as.Date),
      fin_vyuct_narodni_verejne = fin_vyuct_czv - fin_vyuct_soukr - fin_vyuct_eu,
      fin_vyuct_narodni = fin_vyuct_czv - fin_vyuct_eu)

  return(efs_zop)
}


load_efs_prj <- function(dir, filename) {

  path <- file.path(dir, filename)

  efs_prj0 <- read_excel(path, skip = 1) %>%
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
           katekon_kod = kod,
           katekon_nazev = nazev,
           dt_zadost = prvniho_prechodu_do_stavu_pp20,
           dt_smlouva = podpisu_smlouvy,
           dt_ukon_fyz = fyzickeho_ukonceni,
           dt_ukon_cert = financniho_ukonceni_projektu_certifikace_pp42,
           dt_ukon_uzavr = finalniho_uzavreni_projektu_pp43,
           dt_ukon_fyz_predpokl = predpokladane_datum_ukonceni_fyzicke_realizace_projektu_21,
           dt_zahaj_fyz_predpokl = predpokladane_datum_ukonceni_fyzicke_realizace_projektu_20
    ) %>%
    mutate(across(starts_with("dt_"), as.Date)) %>%
    drop_na(prj_id) %>%
    add_op_labels()
  return(efs_prj)
}

load_efs_obl <- function(dir, filename) {
  path <- file.path(dir, filename)

  efs_obl0 <- read_excel(path, skip = 0, col_types = c("text", rep("text", 3),
                                                       "text", "text",
                                                       "numeric", "numeric")) %>%
    janitor::clean_names()

  efs_obl <- efs_obl0 %>%
    rename(prj_id = registracni_cislo_projektu,
           oblast_intervence_podil = oblast_intervence_procentni_podil,
           fin_pravniakt_eu = fin_prostredky_eu,
           impl_stav_id = kod_stavu,
           impl_stav_nazev = nazev_stavu) %>%
    group_by(prj_id, oblast_intervence_kod, oblast_intervence_nazev, impl_stav_nazev,
             impl_stav_id) %>%
    summarise(across(contains("fin_"), sum, na.rm = T), .groups = "drop") %>%
    add_count(prj_id, wt = fin_pravniakt_eu, name = "total") %>%
    mutate(oblast_intervence_podil = fin_pravniakt_eu/total) %>%
    arrange(oblast_intervence_podil) %>%
    select(-total)

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

summarise_zop <- function(efs_zop, quarterly) {
  zop_prep <- efs_zop %>%
    mutate(dt_zop_rok = year(dt_zop_proplaceni),
           dt_zop_rok = if_else(is.na(dt_zop_rok),
                                as.double(str_extract(zop_cislo,
                                                      "(?<=/)20[12][0-9](?=/)")),
                                dt_zop_rok))

  if(quarterly) {
    zop_grp <- zop_prep %>%
      mutate(dt_zop_kvartal = month(dt_zop_proplaceni) %/% 4 + 1) %>%
      group_by(prj_id, dt_zop_rok, dt_zop_kvartal)
  } else {
    zop_grp <- zop_prep %>%
      group_by(prj_id, dt_zop_rok)
  }
  zop <- zop_grp %>%
    summarise(across(starts_with("fin_"), sum, na.rm = T), .groups = "drop")

  # fill in date for ZOP with no date
  # and spread the payment across quarters in that year

  if(quarterly) {
    zop_withq <- zop %>% filter(!is.na(dt_zop_kvartal)) %>%
      mutate(timing_inferred = FALSE)
    zop_noq   <- zop %>% filter( is.na(dt_zop_kvartal))

    # generate all quarters between start of programming period and now
    x <- seq.Date(as.Date("2014-01-01"), Sys.Date(), "quarter")
    qrtrs <- tibble(dt_zop_rok = year(x), dt_zop_kvartal = quarter(x))

    spread_over_quarters <- function(prj_rows, quarters) {

      qrtrs_local <- quarters %>% filter(dt_zop_rok %in% prj_rows$dt_zop_rok)

      # create rows for each quarter in each year
      prj_rows %>%
        select(-dt_zop_kvartal) %>%
        full_join(qrtrs_local, by = "dt_zop_rok") %>%
        group_by(dt_zop_rok) %>%
        # split the payment amount across the rows
        mutate(across(starts_with("fin_vyuct"),
                      ~.x/length(unique((dt_zop_kvartal)))))
    }

    zop_addedq <- zop_noq %>%
      ungroup() %>%
      group_split(prj_id) %>%
      # take each project, and split the payments of each year into rows
      map_dfr(~spread_over_quarters(.x, qrtrs)) %>%
      mutate(timing_inferred = TRUE)

    zop <- bind_rows(zop_addedq, zop_withq)%>%
      group_by(prj_id, dt_zop_rok, dt_zop_kvartal, timing_inferred) %>%
      # sum in case some projects had both missing-date and non-missing-date
      # payments in the same year
      summarise(across(starts_with("fin_vyuct"), sum, na.rm = T),
                .groups = "drop") %>%
      mutate(dt_zop_kvartal_datum = make_date(dt_zop_rok, dt_zop_kvartal * 3 - 2, 1))
  }
  return(zop)
}

load_prv <- function(path, cis_kraj) {
  readxl::read_excel(path) %>%
    janitor::clean_names() %>%
    mutate(opatreni_new = recode(opatreni,
                                 "G" = "16",
                                 "J" = "19",
                                 "K" = "20",
    ),
    operace_new = if_else(is.na(operace), "", operace)) %>%
    unite(opatreni_new, podopatreni, operace_new, col = "prv_operace_kod", sep = ".") %>%
    mutate(across(starts_with("dt_"), as.Date)) %>%
    mutate(dt_zop_rok = year(datum_platby),
           dt_zop_kvartal = month(datum_platby) %/% 4 + 1,
           prv_operace_kod = str_remove(prv_operace_kod, "\\.$")) %>%
    rename(prj_id = registracni_cislo,
           p_id = jednotny_identifikator_prijemce,
           prj_nazev = nazev_projektu,
           p_nazev = nazev_projektu,
           kraj_nazev = kraj_nuts3_text,
           p_pravniforma = pravni_forma_na_zadosti,
           dt_zadost = datum_prijeti_zadosti_o_dotaci,
           dt_platba = datum_platby,
           okres_nazev = okres_nuts4_text,
           fin_vyuct_eu = zdroje_ezfrv_czk,
           fin_vyuct_czv = zdroje_celkem_czk,
           fin_vyuct_narodni = zdroje_narodni_czk,
    ) %>%
    left_join(cis_kraj %>% select(kraj_nazev = TEXT, kraj_id = CZNUTS),
              by = "kraj_nazev")
}

summarise_prv <- function(efs_prv, quarterly) {
  zop_grp <- efs_prv %>%
    group_by(prj_id, dt_zop_rok, prv_operace_kod, kraj_id)
  if(quarterly) {
    zop_grp <- zop_grp %>%
      group_by(dt_zop_kvartal, .add = TRUE)
  }
  zop_grp %>%
    summarise(across(starts_with("fin_"), sum), .groups = "drop")
}

summarise_by_op <- function(efs_zop_quarterly, efs_prv_quarterly) {
  efs_zop_quarterly <- add_op_labels(efs_zop_quarterly) # fn defined in R/utils.R
  efs_prv_quarterly$op_nazev <- "Program rozvoje venkova"
  efs_prv_quarterly$op_nazev_zkr <- "Program rozvoje venkova"
  efs_prv_quarterly$op_zkr <- "PRV"
  efs_prv_quarterly$op_id <- "YY"

  bind_rows(efs_zop_quarterly, efs_prv_quarterly) %>%
    group_by(across(starts_with("op_")), dt_zop_rok) %>%
    summarise(across(starts_with("fin_"), sum), .groups = "drop")
}
