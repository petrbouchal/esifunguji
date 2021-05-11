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
           starts_with("npr"),
           sc_nazev_hier = spec_cil_nazev, eu20_id = evropa_2020, tc_id = tc)

  if (!all(efs_prj$sc_id %in% ef_hier$sc_id)) warning("Some SOs in data not found in hierarchy")

  return(ef_hier)
}
