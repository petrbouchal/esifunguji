load_macrocat_quest <- function(quest_path, prv) {
  q <- readxl::read_excel(quest_path,
                          skip = 0) %>%
    janitor::clean_names()

  if (prv) {
    rename(q, prv_operace = id_operace) %>%
      separate(prv_operace, into = c("prv_operace_kod", "prv_operace_nazev"),
               sep = " ", extra = "merge") %>%
      select(starts_with("prv_operace"), quest_class)
  }  else {
    rename(q, quest_class = class)
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

compile_ef <- function(efs_macrocat, eo_kraj, efs_zop_annual) {
  efs_macrocat %>%
    full_join(eo_kraj, by = "prj_id") %>%
    full_join(efs_zop_annual, by = "prj_id") %>%
    mutate(across(starts_with("fin_"), ~.x * hermin_podil * kraj_podil_wtpocetobyv))
}

compile_ef_prv <- function(efs_prv, macrocat_quest_prv) {
  efs_prv %>%
    left_join(macrocat_quest_prv, by = "prv_operace_kod")
}

summarise_macro <- function(prv, other, quarterly) {
  bnd <- bind_rows(prv, other)
  grp <- bnd %>%
    group_by(zop_rok, kraj_id, quest_class, hermin_class)
  if (quarterly) {
    grp <- group_by(grp, zop_kvartal, .add = TRUE)
  }
  summarise(grp, across(starts_with("fin_"), sum, na.rm = TRUE), .groups = "drop")
}
