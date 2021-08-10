library(magrittr)
library(dplyr)
library(stringr)
op_labels <- tibble::tribble(
  ~op_id,                                                ~op_nazev,    ~op_zkr,
  "01", "Operační program Podnikání a inovace pro konkurenceschopnost", "OP PIK",
  "02",                  "Operační program Výzkum, vývoj a vzdělávání", "OP VVV",
  "03",                                "Operační program Zaměstnanost", "OP Z",
  "04",                                     "Operační program Doprava", "OP D",
  "05",                           "Operační program Životní prostředí", "OP ŽP",
  "06",                      "Integrovaný regionální operační program", "IROP",
  "07",                        "Operační program Praha - pól růstu ČR", "OP PPR",
  "08",                             "Operační program Technická pomoc", "OP TP",
  "11",                        "INTERREG V-A Česká republika - Polsko", "OP ČR-PL"
) %>%
  mutate(op_nazev_zkr = str_replace(op_nazev, "[Oo]perační program|INTERREG V-A", "OP") %>%
           str_replace("Česká republika", "ČR"))

add_op_labels <- function(data, abbrevs = op_labels,
                          drop_orig = TRUE, drop_duplicate_cols = T) {

  if(!"op_id" %in% names(data) & "prj_id" %in% names(data)) {
    data$op_id <- str_sub(data$prj_id, 4, 5)
  } else if ("op_zkr" %in% names(data)) {
    if(drop_orig) data$op_zkr <- NULL else data <- rename(data, op_zkr_orig = op_zkr)
  } else if ("op_nazev" %in% names(data)) {
    if(drop_orig) data$op_nazev <- NULL else data <- rename(data, op_nazev_org = op_nazev)
  }

  data2 <- data %>%
    left_join(abbrevs, by = "op_id", suffix = c("", "_lblx"))

  if(drop_duplicate_cols) data2 <- data2 %>% select(-ends_with("lblx"))

  return(data2)
}

nastroj_op <- tibble::tribble(~nastroj_id, ~op_id,
                      "101",       "XX",
                      "102",       "01",
                      "103",       "02",
                      "104",       "05",
                      "105",       "04",
                      "106",       "05",
                      "107",       "06",
                      "108",       "07",
                      "109",       "08",
                      "110",       "11",
                      "130",       "YY",
                      "187",       "YY")

extract_cl <- function(cl_target_list, cl_string) {

  cl_branchname <- names(cl_target_list)[str_detect(names(cl_target_list),
                                                  paste0(cl_string, "$"))]
  cl <- cl_target_list[[cl_branchname]]
  return(cl)
}


render_readme <- function(path = "README.Rmd", output_format = "github_document", output_file = "README.md") {
  rmarkdown::render(path, output_format, output_file)
}

gh_url <- function(path, repo, user = "petrbouchal", remote = NULL, branch = "main") {
  if(is.null(remote)) rmt <- paste0(user, "/", repo)
  paste("https://github.com", rmt, "tree", branch, path, sep = "/")
}

gh_link <- function(text = NULL, path, repo, user = "petrbouchal", remote = NULL, branch = "main") {
  url <- gh_url(path, repo, user, remote, branch)
  if(is.null(text)) text <- url

  paste0("[", text, "](", url, ")")

}
