#' Check that an object is numeric.
#'
#' Check that an object is numeric and if there are NAs. Message if NAs, and
#' error if not numeric.
#'
#' @param dat List of objects. Message if NAs; error if not numeric.
#' @template param-quiet
#' @importFrom Rdpack reprompt
#' @return NULL.
#' @family check functions
#' @export
#' @examples
#' check_numeric(dat = list(x = c(1:3, NA), y = 1))
check_numeric <- function(dat,
                          quiet = FALSE) {
  # Ensure input is a list
  if (!is.list(dat)) stop("`dat` must be a list", call. = FALSE)
  # Check for NA
  any_na <- vapply(
    X = dat, FUN = function(x) any(is.na(x)),
    FUN.VALUE = vector(mode = "logical", length = 1)
  )
  # If NAs
  if (any(any_na) && !quiet) {
    # Where are the NAs
    na_names <- names(dat)[any_na]
    # Message
    message("NA(s) in: ", paste_nicely(na_names, do_quotes = TRUE), ".")
  } # End if NAs
  # Check for numeric
  not_numeric <- vapply(
    X = dat, FUN = function(x) !is.numeric(x),
    FUN.VALUE = vector(mode = "logical", length = 1)
  )
  # If not numeric
  if (any(not_numeric)) {
    # Names of not numeric
    numeric_names <- names(dat)[not_numeric]
    # Error
    stop("Not numeric: ", paste_nicely(numeric_names, do_quotes = TRUE), ".",
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
#' @template param-quiet
#' @importFrom tibble is_tibble
#' @importFrom Rdpack reprompt
#' @return NULL.
#' @family check functions
#' @export
#' @examples
#' check_tibble(dat = list(x = tibble::tibble(), y = tibble::tibble(x = 1)))
check_tibble <- function(dat,
                         min_rows = 1,
                         quiet = FALSE) {
  # Ensure input is a list
  if (!is.list(dat)) stop("`dat` must be a list", call. = FALSE)
  # Check input: numeric
  check_numeric(dat = list(min_rows = min_rows), quiet = quiet)
  # Check for enough rows
  few_rows <- vapply(
    X = dat, FUN = function(x) nrow(x) < min_rows,
    FUN.VALUE = vector(mode = "logical", length = 1)
  )
  # If not enough rows
  if (any(few_rows) && !quiet) {
    # Where are there not enough rows
    few_row_names <- names(dat)[few_rows]
    # Error
    message("Not enough rows in: ", paste_nicely(few_row_names,
      do_quotes = TRUE
    ), ".")
  } # End if not enough rows
  # Check for tibbles
  not_tibble <- vapply(
    X = dat, FUN = function(x) !is_tibble(x),
    FUN.VALUE = vector(mode = "logical", length = 1)
  )
  # If not tibbles
  if (any(not_tibble)) {
    # Names of not tibbles
    not_tibble_names <- names(dat)[not_tibble]
    # Message
    stop("Not tibble(s): ", paste_nicely(not_tibble_names, do_quotes = TRUE),
      ".",
      call. = FALSE
    )
  } # End if not tibbles
  # Nothing to return
  NULL
}

#' Check a `where' object.
#'
#' Check that a `where' object is a list of characters with required names.
#' Error if this is not the case.
#'
#' @param dat List. Where object: list of characters with names (see examples).
#' @param dat_names Character vector. Names of list items.
#' @importFrom Rdpack reprompt
#' @return NULL.
#' @family check functions
#' @export
#' @examples
#' check_where(dat = list(x = "one", y = "two"), dat_names = c("x", "y"))
check_where <- function(dat,
                        dat_names) {
  # Check dat: list
  if (!is.list(dat)) stop("Argument `where` must be a list.", call. = FALSE)
  # Check dat: names
  if (any(names(unlist(dat)) != dat_names)) {
    stop("Argument `where` needs names:", dat_names, call. = FALSE)
  }
  # Check dat: contents
  if (typeof(unlist(dat)) != "character") {
    stop("Argument `where` must contain characters", call. = FALSE)
  }
  # Nothing to return
  NULL
}
