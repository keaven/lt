#' View a data frame as an interactive table in the default browser
#'
#' Convert a data frame to an [lt()] table, enable [lt_interactive()] controls,
#' and open an HTML file in the default browser. An existing
#' `lt_tbl` keeps its formatting and other table operations. Its existing
#' interactive settings are kept when `...` is empty; otherwise `...` configures
#' the controls. Browser zoom can be used to change the displayed font size.
#'
#' @param data A data frame or an existing `lt_tbl`.
#' @param ... Arguments passed to [lt_interactive()], such as `filter`,
#'   `pager`, `resize`, and `hide`.
#' @param file An HTML output path. By default, a temporary file is used;
#'   supply a path to keep the page.
#' @param browser A browser command or function passed to [utils::browseURL()].
#'   Defaults to the browser configured by R.
#' @return The absolute HTML file path, invisibly.
#' @export
#' @examples
#' # Opens the browser when run interactively:
#' if (interactive()) view_lt(mtcars, filter = TRUE, pager = FALSE)
view_lt = function(
  data, ..., file = tempfile('lt-view-', fileext = '.html'),
  browser = getOption('browser')
) {
  if (!is.character(file) || length(file) != 1L || is.na(file) ||
      !grepl('\\.html$', file, ignore.case = TRUE))
    stop('`file` must be a single .html path.')
  x = if (inherits(data, 'lt_tbl')) data else lt(data)
  if (is.null(x$interactive) || ...length()) x = lt_interactive(x, ...)
  lt_export(x, file, method = 'raw')
  file = normalizePath(file, mustWork = TRUE)
  utils::browseURL(file, browser = browser)
  invisible(file)
}
