#' Print the project variables table to the console
#'
#' Reads \code{data/_variables.csv} from the rENM project directory and prints
#' its contents with a timestamped header. Intended as a lightweight inspection
#' utility for verifying environmental variable definitions before running the
#' pipeline.
#'
#' @param project_dir Character. Path to the rENM project root. If \code{NULL}
#'   (default), resolved via \code{\link{rENM_project_dir}} (argument, \code{rENM.project_dir} option, \code{RENM_PROJECT_DIR} environment
#'   variable).
#'
#' @return Invisibly returns the variables data frame. Called primarily for the
#'   side effect of printing a formatted table and header to the console.
#'
#' @seealso \code{\link{rENM_project_dir}}, \code{\link{show_species}}
#'
#' @importFrom utils read.csv flush.console
#'
#' @examples
#' \dontrun{
#' show_variables()
#' show_variables(project_dir = "/projects/rENM")
#' }
#'
#' @export
show_variables <- function(project_dir = NULL) {

  ## ---- resolve project directory and locate file ----------------------------
  project_root <- rENM_project_dir(project_dir)
  vars_fp      <- .expand(file.path(project_root, "data", "_variables.csv"))

  if (!file.exists(vars_fp)) {
    stop(
      "Variables file not found at: ", vars_fp, "\n",
      "Expected location: <project_dir>/data/_variables.csv",
      call. = FALSE
    )
  }

  ## ---- console header -------------------------------------------------------
  .catln(.sep_line())
  .catln("show_variables()")
  .catln("Timestamp: ", .now())
  .catln("File: ", vars_fp)
  .catln(.sep_line())

  ## ---- read CSV -------------------------------------------------------------
  df <- tryCatch(
    utils::read.csv(vars_fp, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      stop("Failed to read _variables.csv: ", conditionMessage(e), call. = FALSE)
    }
  )

  ## ---- print ----------------------------------------------------------------
  .catln(sprintf("Rows: %d | Columns: %d", nrow(df), ncol(df)))
  .catln(.sep_line())
  print(format(df, justify = "left"), row.names = FALSE, right = FALSE)
  .catln(.sep_line())
  .catln("Done.")
  .catln(.sep_line())

  ## ---- return ---------------------------------------------------------------
  invisible(df)
}
