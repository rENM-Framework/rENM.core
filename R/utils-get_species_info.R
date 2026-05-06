#' Look up species metadata by four-letter alpha code
#'
#' Reads the project species table (\code{data/_species.csv}) and returns a
#' one-row data frame of standardized metadata for the requested species.
#' Matching is case-insensitive. Column names in the source file are normalized
#' before lookup, so minor variations (e.g., \code{ALPHA.CODE} vs
#' \code{SPECIESCODE}) are handled automatically.
#'
#' @details
#' \strong{Alpha-code column detection}
#'
#' The function searches for the alpha-code column by normalizing all column
#' names to uppercase with punctuation removed, then matching against these
#' candidates in order: \code{ALPHACODE}, \code{ALPHA}, \code{SPECIESCODE},
#' \code{SPSC}, \code{BANDINGCODE}. The first match wins. If none is found,
#' the function errors with the list of available columns.
#'
#' \strong{Output column standardization}
#'
#' Regardless of source column names, the returned data frame always uses
#' these names: \code{COMMON.NAME}, \code{SCIENTIFIC.NAME}, \code{EBD.RECORDS},
#' \code{EBD.RANGE}, \code{GAP.RANGE}. The same normalization logic is used to
#' locate these columns in the source file.
#'
#' @param alpha_code Character scalar. Four-letter species alpha code (e.g.,
#'   \code{"CASP"}). Case-insensitive; leading/trailing whitespace is trimmed.
#'
#' @param project_dir Character. Path to the rENM project root. If \code{NULL}
#'   (default), resolved via \code{\link{rENM_project_dir}} (argument, \code{rENM.project_dir} option, \code{RENM_PROJECT_DIR} environment
#'   variable).
#'
#' @return A one-row data frame with columns \code{COMMON.NAME},
#'   \code{SCIENTIFIC.NAME}, \code{EBD.RECORDS}, \code{EBD.RANGE}, and
#'   \code{GAP.RANGE}. Row names are reset to \code{NULL}.
#'
#' @seealso \code{\link{rENM_project_dir}}, \code{\link{show_species}}
#'
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' info <- get_species_info("CASP")
#' info <- get_species_info("CASP", project_dir = "/projects/rENM")
#' }
#'
#' @export
get_species_info <- function(alpha_code, project_dir = NULL) {

  # ---------------------------------------------------------------------------
  # Validate input
  # ---------------------------------------------------------------------------
  if (!is.character(alpha_code) || length(alpha_code) != 1L || !nzchar(alpha_code)) {
    stop("`alpha_code` must be a non-empty character(1).", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Resolve project directory and locate species table
  # ---------------------------------------------------------------------------
  project_root <- rENM_project_dir(project_dir)
  csv_path     <- file.path(project_root, "data", "_species.csv")

  if (!file.exists(csv_path)) {
    stop("Species CSV not found at: ", csv_path, call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Read species table
  # ---------------------------------------------------------------------------
  df <- tryCatch(
    utils::read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      stop("Failed to read CSV at ", csv_path, ": ", conditionMessage(e), call. = FALSE)
    }
  )

  if (!nrow(df)) {
    stop("The species table is present but empty: ", csv_path, call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Column-name normalization: uppercase, punctuation stripped.
  # Allows "COMMON.NAME", "Common Name", "common_name" to all match.
  # ---------------------------------------------------------------------------
  norm     <- function(x) gsub("[^A-Z0-9]", "", toupper(x))
  col_norm <- norm(names(df))

  # ---------------------------------------------------------------------------
  # Locate alpha-code column
  # ---------------------------------------------------------------------------
  # Candidates must stay in sync with @details documentation above.
  alpha_candidates <- c("ALPHACODE", "ALPHA", "SPECIESCODE", "SPSC", "BANDINGCODE")
  alpha_col_idx    <- match(TRUE, col_norm %in% alpha_candidates, nomatch = 0L)

  if (alpha_col_idx == 0L) {
    stop(
      "Could not locate the alpha-code column. Looked for one of: ",
      paste(alpha_candidates, collapse = ", "),
      ". Available columns are: ", paste(names(df), collapse = ", "),
      call. = FALSE
    )
  }

  alpha_col <- names(df)[alpha_col_idx]

  # ---------------------------------------------------------------------------
  # Locate and validate required output columns
  # ---------------------------------------------------------------------------
  wanted      <- c("COMMON.NAME", "SCIENTIFIC.NAME", "EBD.RECORDS", "EBD.RANGE", "GAP.RANGE")
  wanted_norm <- norm(wanted)
  match_idx   <- match(wanted_norm, col_norm, nomatch = 0L)

  if (any(match_idx == 0L)) {
    stop(
      "Missing expected column(s) in species table: ",
      paste(wanted[match_idx == 0L], collapse = ", "),
      ". Available columns are: ", paste(names(df), collapse = ", "),
      call. = FALSE
    )
  }

  actual_wanted <- names(df)[match_idx]

  # ---------------------------------------------------------------------------
  # Filter by alpha code
  # ---------------------------------------------------------------------------
  key     <- toupper(trimws(alpha_code))
  matches <- df[toupper(trimws(df[[alpha_col]])) == key, , drop = FALSE]

  if (nrow(matches) == 0L) {
    stop("No species found for alpha code '", alpha_code, "'.", call. = FALSE)
  }

  if (nrow(matches) > 1L) {
    stop(
      "Multiple rows matched alpha code '", alpha_code, "'. ",
      "Alpha codes must be unique in the species table.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Return standardized one-row data frame
  # ---------------------------------------------------------------------------
  out           <- matches[, actual_wanted, drop = FALSE]
  names(out)    <- wanted
  rownames(out) <- NULL

  out
}
