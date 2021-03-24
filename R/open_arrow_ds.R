##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param files
##' @param path
##' @return
##' @author Petr Bouchal
##' @export
collect_filtered_arrow_ds <- function(files, path) {
  # note the files param is there just to make this step depend on the
  # target watching files inside the Arrow directory
  is.character(files)
  x <- arrow::open_dataset(path)
  x %>%
    filter(per_yr == 2019) %>%
    collect()

}
