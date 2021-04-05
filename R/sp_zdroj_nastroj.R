
budget_new_ops <- function(arrow_path, nastroj_op, codelists) {
  cn <- open_dataset(arrow_path)

  # print(names(codelists))

  zdroj_branchname <- names(codelists)[str_detect(names(codelists), "zdroj$")]
  zdroj <- codelists[[zdroj_branchname]] %>%
    rename(zdroj = zdroj_id)

  nastroj_branchname <- names(codelists)[str_detect(names(codelists), "nastroj$")]
  nastroj <- codelists[[nastroj_branchname]]

  cnz <- cn %>%
    filter(druh == "Výdaje") %>%
    select(trida, seskupeni, podseskupeni, zdroj, budget_spending, period_vykaz) %>%
    collect() %>%
    sp_add_codelist(zdroj) %>%
    sp_add_codelist(nastroj) %>%
    left_join(nastroj_op, by = "nastroj_id") %>% # set manually in utils.R
    add_op_labels()

  return(cnz)
}
