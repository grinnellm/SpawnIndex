#' Check values for NA or not numeric.
#'
#' Check values for NA or not numeric. Gives a message if there are NA(s), and
#' and error if it's not numeric.
#'
#' @param dat List. Message if NAs present; error if is not numeric.
#' @param quiet Logical. Suppress messages; default is FALSE.
#' @importFrom Rdpack reprompt
#' @return NULL.
#' @family helper functions
#' @export
#' @examples
#' alpha <- c(1:3, NA)
#' beta <- "a"
#' check_numeric(dat = list(alpha = alpha, beta = beta))
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
    # Where are the non-numeric
    numeric_names <- names(dat)[sapply(dat, function(x) !is.numeric(x))]
    # Error
    stop("Non-numeric values: ", paste(numeric_names, collapse = ", "), ".",
      call. = FALSE
    )
  } # End numeric check
  # Nothing to return
  NULL
}

#' Check that an object is a tibble.
#'
#' Check that an object is a tibble and has a minimum number of rows. Gives a
#' message if there are fewer rows, and an error if it's not a tibble.
#'
#' @param dat List. Message if no rows; stop if not a tibble.
#' @param quiet Logical. Suppress messages; default is FALSE.
#' @importFrom Rdpack reprompt
#' @return NULL.
#' @family helper functions
#' @export
#' @examples
#' alpha <- data.frame(a = 1, b = 2)
#' beta <- tibble()
#' check_tibble(dat = list(alpha = alpha, beta = beta))
check_tibble <- function(dat, min_rows = 1, quiet = FALSE) {
  # Ensure input is a list
  if (!is.list(dat)) stop("`dat` must be a list", call. = FALSE)
  # Check for rows
  if (any(nrow(unlist(dat))) <= min_rows) {
    # Where are the zero rows
    no_row_names <- names(dat)[sapply(dat, function(x) nrow(x) == 0)]
    # Error
    message("No data in: ", paste(no_row_names, collapse = ", "), ".")
  } # End row check
  # Check for tibble
  if (any(!is_tibble(unlist(dat))) & !quiet) {
    # Where are the non-tibbles
    not_tibble_names <- names(dat)[sapply(dat, function(x) !is_tibble(x))]
    # Message
    stop("Not tibble(s): ", paste(not_tibble_names, collapse = ", "), ".",
      call. = FALSE
    )
  } # End tibble check
  # Nothing to return
  NULL
}
