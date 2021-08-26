load_hierarchy <- function(path, sheet) {
  hier <- read_excel(path, sheet, skip = 3) %>%
    clean_names() %>%
    mutate_if(is.character, ~str_trim(str_squish(.x))) %>%
    mutate_at(vars(evropa_2020, tc), ~na_if(.x, "0")) %>%
    mutate(tc = str_replace(tc, "TC ", "TC"))
}

process_hierarchy <- function(ef_hier, efs_prj) {
  ef_hier <- ef_hier %>%
    select(sc_id = spec_cil_kod,
           # starts_with("npr"),
           sc_nazev_hier = spec_cil_nazev, eu20_id = evropa_2020, tc_id = tc) %>%
    mutate(across(starts_with("npr_"), ~str_remove(.x, "\\.$"))) %>%
    distinct() %>%
    group_by(sc_id) %>%
    # deduplicate where there are two rows for one SC with slight diffs in another column
    filter(row_number() == 1) %>%
    ungroup()

  if (!all(efs_prj$sc_id %in% ef_hier$sc_id)) warning("Some SOs in data not found in hierarchy")

  return(ef_hier)
}

summarise_by_hierarchy <- function(efs_zop_annual, efs_prj_sc, ef_hier) {
  efs_sum_sc <- efs_zop_annual %>%
    left_join(efs_prj_sc) %>%
    mutate(across(starts_with("fin_"), ~ .x * sc_podil)) %>%
    group_by(sc_id, sc_nazev) %>%
    summarise(across(starts_with("fin_"), sum, na.rm = T), .groups = "drop")

  efs_hier <- ef_hier %>%
    full_join(efs_sum_sc)

}
