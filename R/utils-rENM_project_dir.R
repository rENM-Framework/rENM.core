#' Resolve and validate the rENM project root directory
#'
#' Returns a normalized absolute path to the rENM project root, resolving it
#' from the first available source in this precedence order: the explicit
#' \code{project_dir} argument, the \code{rENM.project_dir} R option, or the
#' \code{RENM_PROJECT_DIR} environment variable. Errors with an actionable
#' message if no valid directory can be found.
#'
#' @details
#' This function is the single point of filesystem configuration for the rENM
#' framework. All other functions that access the project root call this one
#' rather than resolving paths independently, keeping hard-coded paths out of
#' the codebase and making scripts portable and CRAN-compliant.
#'
#' A candidate path is accepted only if it is a non-empty scalar character
#' string that expands without error (\code{path.expand}) and refers to a
#' directory that already exists (\code{normalizePath(..., mustWork = TRUE)}).
#' A candidate that fails any check is silently skipped and the next source
#' is tried.
#'
#' \strong{Configuring the project directory}
#' \describe{
#'   \item{Explicit argument (recommended for scripts and tests)}{
#'     Pass the path directly: \code{rENM_project_dir("/projects/rENM")}.}
#'   \item{R option (session-level convenience)}{
#'     \code{options(rENM.project_dir = "~/rENM")}}
#'   \item{Environment variable (persistent across sessions)}{
#'     Add \code{RENM_PROJECT_DIR=~/rENM} to \code{~/.Renviron}, or call
#'     \code{Sys.setenv(RENM_PROJECT_DIR = "~/rENM")} for the current session.}
#' }
#'
#' @param project_dir Character. Path to the rENM project root. Takes
#'   precedence over the R option and environment variable. If \code{NULL}
#'   (default), the option and environment variable are tried in order.
#'
#' @return Character scalar. Normalized absolute path to an existing directory.
#'
#' @seealso \code{\link{get_species_info}}, \code{\link{show_species}},
#'   \code{\link{show_variables}}
#'
#' @examples
#' \dontrun{
#' # Explicit path — best for reproducible scripts and tests
#' rENM_project_dir("/projects/rENM")
#'
#' # Session option
#' options(rENM.project_dir = "~/rENM")
#' rENM_project_dir()
#'
#' # Environment variable
#' Sys.setenv(RENM_PROJECT_DIR = "~/rENM")
#' rENM_project_dir()
#' }
#'
#' @export
rENM_project_dir <- function(project_dir = NULL) {

  # Internal helper: validate, expand, and normalize a candidate path.
  # Returns NULL if the candidate is missing, malformed, or does not exist,
  # allowing the caller to fall through to the next configuration source.
  resolve <- function(p) {
    if (is.null(p)) return(NULL)
    if (!is.character(p) || length(p) != 1) return(NULL)
    if (is.na(p) || !nzchar(p)) return(NULL)
    normalizePath(path.expand(p), mustWork = TRUE)
  }

  # 1) Explicit argument (most reproducible; best for scripts/tests)
  p <- resolve(project_dir)
  if (!is.null(p)) return(p)

  # 2) Package option (session-level convenience)
  p <- resolve(getOption("rENM.project_dir"))
  if (!is.null(p)) return(p)

  # 3) Environment variable (can be made persistent via .Renviron)
  p <- resolve(Sys.getenv("RENM_PROJECT_DIR", unset = NA_character_))
  if (!is.null(p)) return(p)

  stop(
    "rENM project directory not found.\n",
    "Provide `project_dir =` or set one of:\n",
    "  options(rENM.project_dir = '/path/to/rENM')\n",
    "  Sys.setenv(RENM_PROJECT_DIR = '/path/to/rENM')",
    call. = FALSE
  )
}
