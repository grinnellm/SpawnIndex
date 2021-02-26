#' Check that an object is numeric.
#'
#' Check that an object is numeric and if there are NAs. Message if NAs, and
#' error if not numeric.
#'
#' @param dat List of objects. Message if NAs; error if not numeric.
#' @param quiet Logical. Suppress messages; default is FALSE.
#' @importFrom Rdpack reprompt
#' @return NULL.
#' @family checker functions
#' @export
#' @examples
#' check_numeric(dat = list(x = c(1:3, NA), y = 1))
check_numeric <- function(dat, quiet = FALSE) {
  # Ensure input is a list
  if (!is.list(dat)) stop("`dat` must be a list", call. = FALSE)
  # Check for NA
  if (any(is.na(unlist(dat))) & !quiet) {
    # Where are the NAs
    na_names <- names(dat)[sapply(dat, function(x) any(is.na(x)))]
    # Message
    message("NA(s) in: ", paste(na_names, collapse = ", "), ".")
  } # End NA check
  # Check for numeric
  if (!is.numeric(unlist(dat))) {
    # Names of not numeric
    numeric_names <- names(dat)[sapply(dat, function(x) !is.numeric(x))]
    # Error
    stop("Not numeric: ", paste(numeric_names, collapse = ", "), ".",
      call. = FALSE
    )
  } # End numeric check
  # Nothing to return
  NULL
}

#' Check that an object is a tibble.
#'
#' Check that an object is a tibble and has a minimum number of rows. Message if
#' fewer rows, and error if not a tibble.
#'
#' @param dat List of objects. Message if fewer rows; error if not a tibble.
#' @param min_rows Numeric. Minimum number of rows in the tibble; default is 1.
#' @param quiet Logical. Suppress messages; default is FALSE.
#' @importFrom tibble is_tibble
#' @importFrom Rdpack reprompt
#' @return NULL.
#' @family checker functions
#' @export
#' @examples
#' check_tibble(dat = list(x = tibble::tibble(), y = tibble::tibble(x = 1)))
check_tibble <- function(dat, min_rows = 1, quiet = FALSE) {
  # Ensure input is a list
  if (!is.list(dat)) stop("`dat` must be a list", call. = FALSE)
  # Check input: numeric
  check_numeric(dat = list(min_rows = min_rows), quiet = quiet)
  # Check for rows
  no_rows <- sapply(dat, function(x) nrow(x) < min_rows)
  # If no rows
  if (any(no_rows) & !quiet) {
    # Where are the zero rows
    no_row_names <- names(dat)[no_rows]
    # Error
    message("No rows in: ", paste(no_row_names, collapse = ", "), ".")
  } # End row check
  # Check for tibble
  not_tibble <- sapply(dat, function(x) !is_tibble(x))
  # If not tibbles
  if (any(not_tibble)) {
    # Names of not tibbles
    not_tibble_names <- names(dat)[not_tibble]
    # Message
    stop("Not tibble(s): ", paste(not_tibble_names, collapse = ", "), ".",
      call. = FALSE
    )
  } # End tibble check
  # Nothing to return
  NULL
}
