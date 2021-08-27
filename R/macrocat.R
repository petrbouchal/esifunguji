load_macrocat_quest <- function(quest_path, prv) {
  q <- readxl::read_excel(quest_path,
                          skip = 0) %>%
    janitor::clean_names()

  if (prv) {
    rename(q, prv_operace = id_operace) %>%
      separate(prv_operace, into = c("prv_operace_kod", "prv_operace_nazev"),
               sep = " ", extra = "merge") %>%
      replace_na(list(hermin_type = "")) %>%
      mutate(hermin_class = if_else(quest_class != "AIS",
                                    quest_class,
                                    # for PRV, we have QUEST and HERMIN
                                    # categorisations in one file
                                    paste0(quest_class, hermin_type))) %>%
      select(starts_with("prv_operace"), quest_class, hermin_class)
  }  else {
    q %>%
      mutate(class = if_else(is.na(class_new), class, class_new)) %>%
      rename(quest_class = class) %>%
      select(-class_new)
  }
}

load_macrocat_hermin <- function(hermin_path) {

  h <- readxl::read_excel(hermin_path,
                          skip = 1) %>%
    janitor::clean_names() %>%
    rename(hermin_type = type, katekon_nazev = nazev)
}

build_efs_macrocat <- function(efs_prj_kat, efs_obl,
                               macrocat_quest,
                               macrocat_hermin) {
  efs_obl %>%
    left_join(efs_prj_kat) %>%
    select(prj_id, starts_with("katekon_"), starts_with("oblast_")) %>%
    left_join(macrocat_quest) %>%
    left_join(macrocat_hermin) %>%
    mutate(hermin_class = if_else(quest_class != "AIS",
                                  quest_class,
                                  paste0(quest_class, hermin_type)),
           hermin_podil = oblast_intervence_podil * katekon_podil) %>%
    filter(hermin_podil > 0)
}

compile_ef <- function(efs_macrocat, eo_kraj, efs_zop_bytime) {
  efs_macrocat %>%
    full_join(eo_kraj, by = "prj_id") %>%
    full_join(efs_zop_bytime, by = "prj_id") %>%
    mutate(across(starts_with("fin_"),
                  ~.x * hermin_podil * kraj_podil_wtpocetobyv,
                  .names = "{.col}_wt_pocetobyv"),
           across(.cols = c(starts_with("fin_"), -contains("_wt_")),
                  ~.x * hermin_podil * kraj_podil_wtpocetkraju,
                  .names = "{.col}_wt_pocetkraju"))
}

compile_ef_prv <- function(efs_prv, macrocat_quest_prv) {
  efs_prv %>%
    left_join(macrocat_quest_prv, by = "prv_operace_kod") %>%
    mutate(hermin_podil = 1) %>%
    mutate(across(starts_with("fin_"),
                  ~.x,
                  .names = "{.col}_wt_pocetobyv"),
           across(.cols = c(starts_with("fin_"), -contains("_wt_")),
                  ~.x,
                  .names = "{.col}_wt_pocetkraju"))
}

summarise_macro <- function(other, prv, quarterly, regional) {
  other$source <- "mssf"
  prv$source <- "prv"

  bnd <- bind_rows(other, prv)

  if(!regional) {
    bnd <- bnd %>%
      ungroup() %>%
      select(prj_id, starts_with("fin_"), -matches("_wt"), matches("^(hermin)|(quest)"),
             starts_with("dt_"), source, oblast_intervence_podil, katekon_podil) %>%
      distinct() %>%
      filter(TRUE)
    # print(names(other))
  }

  grp <- bnd %>%
    group_by(dt_zop_rok, quest_class, hermin_class, source)

  if (quarterly) {
    grp <- group_by(grp, dt_zop_kvartal, dt_zop_kvartal_datum, .add = TRUE)
  }

  if(regional) {

    make_conditional_wt <- function(var, class, var_wt_1, var_wt_2) {
      case_when(class %in% c("RD", "AIS", "TA") ~ var_wt_1,
                class %in% c("INFR", "HC") ~ var_wt_2)
    }

    # warning("REGIONAL!!!")

    grp <- group_by(grp, kraj_id, .add = TRUE)

    rr <- summarise(grp, across(starts_with("fin_"), sum, na.rm = TRUE), .groups = "drop") %>%
      mutate(fin_vyuct_czv_wt_cond = make_conditional_wt(fin_vyuct_czv, quest_class, fin_vyuct_czv_wt_pocetkraju, fin_vyuct_czv_wt_pocetobyv),
             fin_vyuct_eu_wt_cond = make_conditional_wt(fin_vyuct_eu_wt_cond, quest_class, fin_vyuct_eu_wt_pocetkraju, fin_vyuct_eu_wt_pocetobyv),
             fin_vyuct_sr_wt_cond = make_conditional_wt(fin_vyuct_sr_wt_cond, quest_class, fin_vyuct_sr_wt_pocetkraju, fin_vyuct_sr_wt_pocetobyv),
             fin_vyuct_sf_wt_cond = make_conditional_wt(fin_vyuct_sf_wt_cond, quest_class, fin_vyuct_sf_wt_pocetkraju, fin_vyuct_sf_wt_pocetobyv),
             fin_vyuct_obec_wt_cond = make_conditional_wt(fin_vyuct_obec_wt_cond, quest_class, fin_vyuct_obec_wt_pocetkraju, fin_vyuct_obec_wt_pocetobyv),
             fin_vyuct_kraj_wt_cond = make_conditional_wt(fin_vyuct_kraj_wt_cond, quest_class, fin_vyuct_kraj_wt_pocetkraju, fin_vyuct_kraj_wt_pocetobyv),
             fin_vyuct_jine_nar_ver_wt_cond = make_conditional_wt(fin_vyuct_jine_nar_ver_wt_cond, quest_class, fin_vyuct_jine_nar_ver_wt_pocetkraju, fin_vyuct_jine_nar_ver_wt_pocetobyv),
             fin_vyuct_narodni_wt_cond = make_conditional_wt(fin_vyuct_narodni_wt_cond, quest_class, fin_vyuct_narodni_wt_pocetkraju, fin_vyuct_narodni_wt_pocetobyv),
             fin_vyuct_narodni_verejne_wt_cond = make_conditional_wt(fin_vyuct_narodni_verejne_wt_cond, quest_class, fin_vyuct_narodni_verejne_wt_pocetkraju, fin_vyuct_narodni_verejne_wt_pocetobyv),
             fin_vyuct_soukr_wt_cond = make_conditional_wt(fin_vyuct_soukr_wt_cond, quest_class, fin_vyuct_soukr_wt_pocetkraju, fin_vyuct_soukr_wt_pocetobyv)) %>%
      select(-matches("(czv|eu|sr|sf|obec|kraj|soukr|jine_nar_ver|narodni|narodni_verejne)$"))
  } else {
    rr <- grp %>%
      select(-matches("_wt")) %>%
      replace_na(list(oblast_intervence_podil = 1)) %>%
      mutate(across(starts_with("fin_"), ~.x * oblast_intervence_podil * katekon_podil)) %>%
      summarise(across(starts_with("fin_"), sum, na.rm = TRUE), .groups = "drop")
  }

  return(rr)
}

export_table <- function(data, path, fun, ...) {

  fun(data, path, ...)

  return(path)
}
