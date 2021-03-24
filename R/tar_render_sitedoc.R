##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param pth Rmd file
##' @param site_dir
##' @return
##' @author Petr Bouchal
##' @export
tar_render_sitedoc <- function(pth, site_dir = "docs") {
  dochtml <- paste0(tools::file_path_sans_ext(pth), ".html")
  outpath <- file.path(site_dir, dochtml)

  command = {!! tar_knitr_deps_expr(pth)
    siteconf
    sitefiles
    rmarkdown::render_site(pth)
    outpath
  }

  name <- deparse_language(substitute(name))
  envir <- tar_option_get("envir")
  command <- tidy_eval(substitute(command), envir, tidy_eval)

  tar_target_raw(name, command = command, format = "file")
}
