# utils-internal.R
# Internal helper functions shared across rENM.core
#
# Not exported. Downstream packages that need equivalent helpers should
# define their own copies in a utils-internal.R file, following this template.


#' @noRd
.expand <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)

#' @noRd
.now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

#' @noRd
.sep_line <- function(n = 72L) paste(rep.int("-", n), collapse = "")

#' @noRd
.catln <- function(...) { cat(paste0(..., "\n")); flush.console() }
