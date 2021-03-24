##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param xml_path
##' @param out_dir
##' @return
##' @author Petr Bouchal
##' @export
codelist_to_parquet <- function(codelist_df, codelist_name, out_dir) {

  outpath <- here::here("data-processed", out_dir, paste0(codelist_name, ".parquet"))
  arrow::write_parquet(x = codelist_df[[1]], sink = outpath)
  return(outpath)
}

sp_load_codelist_named <- function(xml_path) {

  cl <- sp_load_codelist(xml_path)
  outfile <- str_remove(basename(xml_path), "\\.xml$")

  cl <- list(cl)
  names(cl) <- outfile

  return(cl)
}
