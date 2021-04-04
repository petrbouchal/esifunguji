load_and_prep_budget <- function(table_file, polozka_codelist, months) {

  polozka <- polozka_codelist

  statnipokladna::sp_load_table(table_file) %>%
    sp_add_codelist(polozka) %>%
    group_by(per_yr, ucjed, ico,
             paragraf, polozka, druh, polozka_nazev,
             trida, seskupeni, podseskupeni)
}

load_budget_yearsum_local <- function(table_file, months, codelists) {

  # print(table_file)

  polozka_branchname <- names(codelists)[str_detect(names(codelists), "polozka$")]
  polozka <- codelists[[polozka_branchname]]

  b_base <- load_and_prep_budget(table_file, polozka, months)

  rslt <- b_base %>%
    filter(!kon_pol | !kon_rep | !kon_okr | !kon_kraj) %>%
    filter(per_m %in% months) %>%
    group_by(nuts, kraj, .add = T) %>%
    summarise(across(c(starts_with("budget")), sum, na.rm = T),
              .groups = "drop") %>%
    mutate(period_vykaz = lubridate::make_date(per_yr, 12, 31))
  return(rslt)
}

load_budget_yearsum_central_old <- function(table_file, months, codelists) {

  # print(table_file)

  polozka_branchname <- names(codelists)[str_detect(names(codelists), "polozka$")]
  polozka <- codelists[[polozka_branchname]]

  b_base <- load_and_prep_budget(table_file, polozka, months)

  rslt <- b_base %>%
    filter(!kon_pol | kon_rep | kon_okr | kon_kraj) %>%
    filter(per_m %in% months) %>%
    group_by(kapitola, .add = TRUE) %>%
    summarise(across(c(starts_with("budget")), sum, na.rm = T),
              .groups = "drop") %>%
    mutate(period_vykaz = lubridate::make_date(per_yr, 12, 31))

  return(rslt)
}

load_budget_yearsum_central_new <- function(table_file, months,
                                            codelists) {

  # print(table_file)

  polozka_branchname <- names(codelists)[str_detect(names(codelists), "polozka$")]
  polozka <- codelists[[polozka_branchname]]

  b_base <- load_and_prep_budget(table_file, polozka, months)

  rslt <- b_base %>%
    filter(!kon_pol | kon_rep | kon_okr | kon_kraj) %>%
    group_by(kapitola, zdroj, pvs, .add = TRUE) %>%
    summarise(across(c(starts_with("budget")), sum, na.rm = T),
              .groups = "drop") %>%
    mutate(period_vykaz = lubridate::make_date(per_yr, 12, 31))
  return(rslt)
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param table_file
##' @return
##' @author Petr Bouchal
##' @export
budget_arrow_months <- function(table_file, destination, months, fn, codelists) {

  unlink(destination, recursive = T)
  purrrow::marrow_dir(table_file, fn,
                      months = months, codelists = codelists,
                      .path = here::here(destination),
                      .partitioning = c("per_yr", "period_vykaz", "druh"))

}

load_budget_local_grants <- function(table_file, months, codelists) {

  ucelznak_branchname <- names(codelists)[str_detect(names(codelists), "ucelznak$")]
  ucelznak <- codelists[[ucelznak_branchname]] %>%
    rename(ucelznak = ucelznak_id)

  sp_load_table(table_file) %>%
    sp_add_codelist(ucelznak) %>%
    filter(per_m %in% months) %>%
    rename(budget_grants = ZU_ROZKZM) %>%
    mutate(druh = "Příjmy")
}
