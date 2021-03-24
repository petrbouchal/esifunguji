##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param path
##' @return
##' @author Petr Bouchal
##' @export
read_pubxls <- function(path) {

  esif <- read_excel(path, skip = 2, col_types = "text") %>%
    slice(-1) %>%
    clean_names() %>%
    mutate(ico = str_pad(ic, width = 8, pad = "0"),
           across(starts_with("celkove"), as.numeric),
           across(contains("datum"), ~as.Date(.x, format = "%d.%m.%Y")),
           across(starts_with("financni"), as.numeric))
  return(esif)
}
