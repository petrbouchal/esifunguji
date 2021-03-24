##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param target_name
##' @return
##' @author Petr Bouchal
##' @export
read_tarquet <- function(target_name) {

  a <- tar_read_raw(target_name)
  arrow::read_parquet(a)

}
