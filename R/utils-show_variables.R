#' Show available variable definitions in the data directory
#'
#' Reads and displays the contents of the variables metadata table to
#' support inspection and validation of environmental variables used in
#' the rENM workflow.
#'
#' @details
#' This function is part of the rENM framework's processing pipeline
#' and operates within the project directory structure defined by
#' rENM_project_dir().
#'
#' \strong{Pipeline context}
#' This function is intended as a simple inspection utility to verify
#' available environmental variables (e.g., MERRA-2 or derived variables)
#' used throughout the rENM workflow.
#'
#' \strong{Inputs}
#' The variables file is expected at:
#'
#' \code{<project_dir>/data/_variables.csv}
#'
#' The CSV file is expected to contain metadata describing environmental
#' variables (e.g., variable names, descriptions, units, sources). No
#' strict schema is enforced, but column headers are assumed to be
#' present.
#'
#' \strong{Outputs}
#' The function prints a formatted summary and the full contents of the
#' variables table to the console with left-justified columns.
#'
#' \strong{Methods}
#' This function performs:
#'
#' \itemize{
#'   \item Resolution of the project root directory using
#'   \code{\link{rENM_project_dir}}
#'   \item Location of the variables file in the data directory
#'   \item Reading the CSV file using \code{utils::read.csv}
#'   \item Printing a formatted header and table contents to the console
#' }
#'
#' \strong{Data requirements}
#' The file must exist and be readable as a CSV file. If the file does
#' not exist or cannot be read, the function stops with a clear error
#' message.
#'
#' \strong{Console output}
#' The function prints:
#'
#' \itemize{
#'   \item A header block with timestamp and file path
#'   \item The number of rows and columns detected
#'   \item The full data frame with left-justified columns
#' }
#'
#' This function does not write to the run log (\code{_log.txt}) because
#' it is intended as a lightweight inspection tool.
#'
#' @return
#' Data frame. Invisibly returns the data frame read from
#' \code{_variables.csv}.
#'
#' Side effects:
#' \itemize{
#'   \item Prints formatted output to the console
#'   \item Validates file existence and readability
#' }
#'
#' @importFrom utils read.csv flush.console
#'
#' @examples
#' \dontrun{
#'   # Display all variables defined for the project
#'   vars <- show_variables()
#' }
#'
#' @seealso
#' \link{rENM_project_dir}
#'
#' @export
show_variables <- function() {
  
  ## ---- helpers --------------------------------------------------------------
  .expand   <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)
  .now      <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  .sep_line <- function(n = 72L) paste(rep.int("-", n), collapse = "")
  .catln    <- function(...) { cat(paste0(..., "\n")); flush.console() }
  
  ## ---- resolve project directory --------------------------------------------
  project_dir <- rENM_project_dir()
  
  vars_fp <- .expand(file.path(project_dir, "data", "_variables.csv"))
  
  ## ---- validation -----------------------------------------------------------
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
  df <- try(
    utils::read.csv(vars_fp, stringsAsFactors = FALSE, check.names = FALSE),
    silent = TRUE
  )
  
  if (inherits(df, "try-error")) {
    stop("Failed to read _variables.csv: ", as.character(df), call. = FALSE)
  }
  
  ## ---- summary --------------------------------------------------------------
  n_rows <- nrow(df)
  n_cols <- ncol(df)
  
  .catln(sprintf("Rows: %d | Columns: %d", n_rows, n_cols))
  .catln(.sep_line())
  
  ## ---- print contents -------------------------------------------------------
  df_fmt <- format(df, justify = "left")
  print(df_fmt, row.names = FALSE, right = FALSE)
  
  .catln(.sep_line())
  .catln("Done.")
  .catln(.sep_line())
  
  ## ---- return ---------------------------------------------------------------
  invisible(df)
}