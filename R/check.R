#' Check that an object is numeric.
#'
#' Check that an object is numeric and if there are NAs. Message if NAs, and
#' error if not numeric.
#'
#' @param dat List of objects. Message if NAs; error if not numeric.
#' @param quiet Logical. Suppress messages; default is FALSE.
#' @importFrom Rdpack reprompt
#' @return NULL.
#' @family check functions
#' @export
#' @examples
#' check_numeric(dat = list(x = c(1:3, NA), y = 1))
check_numeric <- function(dat, quiet = FALSE) {
  # Ensure input is a list
  if (!is.list(dat)) stop("`dat` must be a list", call. = FALSE)
  # Check for NA
  any_na <- sapply(X = dat, FUN = function(x) any(is.na(x)))
  # If NAs
  if (any(any_na) & !quiet) {
    # Where are the NAs
    na_names <- names(dat)[any_na]
    # Message
    message("NA(s) in: ", paste(na_names, collapse = ", "), ".")
  } # End if NAs
  # Check for numeric
  not_numeric <- sapply(X = dat, FUN = function(x) !is.numeric(x))
  # If not numeric
  if (any(not_numeric)) {
    # Names of not numeric
    numeric_names <- names(dat)[not_numeric]
    # Error
    stop("Not numeric: ", paste(numeric_names, collapse = ", "), ".",
      call. = FALSE
    )
  } # End if not numeric
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
#' @family check functions
#' @export
#' @examples
#' check_tibble(dat = list(x = tibble::tibble(), y = tibble::tibble(x = 1)))
check_tibble <- function(dat, min_rows = 1, quiet = FALSE) {
  # Ensure input is a list
  if (!is.list(dat)) stop("`dat` must be a list", call. = FALSE)
  # Check input: numeric
  check_numeric(dat = list(min_rows = min_rows), quiet = quiet)
  # Check for enough rows
  few_rows <- sapply(X = dat, FUN = function(x) nrow(x) < min_rows)
  # If not enough rows
  if (any(few_rows) & !quiet) {
    # Where are there not enough rows
    few_row_names <- names(dat)[few_rows]
    # Error
    message("Not enough rows in: ", paste(few_row_names, collapse = ", "), ".")
  } # End if not enough rows
  # Check for tibbles
  not_tibble <- sapply(X = dat, FUN = function(x) !is_tibble(x))
  # If not tibbles
  if (any(not_tibble)) {
    # Names of not tibbles
    not_tibble_names <- names(dat)[not_tibble]
    # Message
    stop("Not tibble(s): ", paste(not_tibble_names, collapse = ", "), ".",
      call. = FALSE
    )
  } # End if not tibbles
  # Nothing to return
  NULL
}
