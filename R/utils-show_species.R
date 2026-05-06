#' Print the project species table to the console
#'
#' Reads \code{data/_species.csv} from the rENM project directory and prints
#' its contents with a timestamped header. Intended as a lightweight inspection
#' utility for verifying species definitions before running the pipeline.
#'
#' @param project_dir Character. Path to the rENM project root. If \code{NULL}
#'   (default), resolved via \code{\link{rENM_project_dir}} (argument, \code{rENM.project_dir} option, \code{RENM_PROJECT_DIR} environment
#'   variable).
#'
#' @return Invisibly returns the species data frame. Called primarily for the
#'   side effect of printing a formatted table and header to the console.
#'
#' @seealso \code{\link{rENM_project_dir}}, \code{\link{get_species_info}},
#'   \code{\link{show_variables}}
#'
#' @importFrom utils read.csv flush.console
#'
#' @examples
#' \dontrun{
#' show_species()
#' show_species(project_dir = "/projects/rENM")
#' }
#'
#' @export
show_species <- function(project_dir = NULL) {

  ## ---- resolve project directory and locate file ----------------------------
  project_root <- rENM_project_dir(project_dir)
  species_fp   <- .expand(file.path(project_root, "data", "_species.csv"))

  if (!file.exists(species_fp)) {
    stop(
      "Species file not found at: ", species_fp, "\n",
      "Expected location: <project_dir>/data/_species.csv",
      call. = FALSE
    )
  }

  ## ---- console header -------------------------------------------------------
  .catln(.sep_line())
  .catln("show_species()")
  .catln("Timestamp: ", .now())
  .catln("File: ", species_fp)
  .catln(.sep_line())

  ## ---- read CSV -------------------------------------------------------------
  df <- tryCatch(
    utils::read.csv(species_fp, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      stop("Failed to read _species.csv: ", conditionMessage(e), call. = FALSE)
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
