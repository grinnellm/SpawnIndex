#' Paste things nicely.
#'
#' Paste things together nicely for printing. Quotes and oxford comma included.
#'
#' @param x Objects to paste.
#' @param int_char Character to put between objects. Default is `", "`.
#' @param n_char Character to put between last two objects. Default is `"and "`.
#' @param do_quotes Logical Add quotations. Default is FALSE (no quotes).
#' @param quotes Character vector of length two (i.e., preceding and following
#'   quotes). Default is c("`", "`").
#' @importFrom Rdpack reprompt
#' @return Character. Objects pasted together.
#' @family utility functions
#' @export
#' @examples
#' paste_nicely(x = letters[1:3])
paste_nicely <- function(x,
                         int_char = ", ",
                         n_char = "and ",
                         do_quotes = FALSE,
                         quotes = c("`", "`")) {
  # Check input: characters
  if (
    !is.character(int_char) || !is.character(n_char) || !is.character(quotes)
  ) {
    stop("`int_char`, `n_char`, and `quotes` must be character.", call. = FALSE)
  }
  # Check input: logical
  if (!is.logical(do_quotes)) {
    stop("`do_quotes` must be logical.", call. = FALSE)
  }
  # Check for two values in quotes
  if (do_quotes && length(quotes) != 2) stop("`quotes` must have two values.")
  # Get the length of the vector
  n <- length(x)
  # If quote
  if (do_quotes) x <- paste(quotes[1], x, quotes[2], sep = "")
  # If there are more than two
  if (n > 2) {
    # Make a print-friendly vector
    x[n] <- paste(n_char, x[n], sep = "")
    # Get print friendly values
    res <- paste(x, collapse = int_char)
  } else { # End if more than two, otherwise
    # Add a space
    n_char_sp <- paste(" ", n_char, sep = "")
    # Get print friendly values
    res <- paste(x, collapse = n_char_sp)
  } # End if not more than two
  # Check results: character
  if (!is.character(res)) stop("`res` must be a character.", call. = FALSE)
  # Return the results
  res
} # End paste_nicely function

#' Calculate the maximum.
#'
#' Calculate the maximum; an alternative to `max(x)` that returns `NA` if `x` is
#' all `NA` or an empty set.
#'
#' @param x A numeric vector.
#' @template param-omit_na
#' @return A numeric vector.
#' @seealso \code{\link{max}}
#' @family utility functions
#' @export
#' @examples
#' max(NA, na.rm = TRUE)
#' max_na(NA)
max_na <- function(x, omit_na = TRUE) {
  # If all NA or empty
  if (all(is.na(x)) || length(x) == 0) {
    # NA
    res <- NA
  } else { # End if all NA or empty, otherwise
    # Maximum
    res <- max(x, na.rm = omit_na)
  } # End if otherwise
  # Return the result
  return(res)
} # End max_na function

#' Calculate the sum.
#'
#' Calculate the sum; an alternative to `sum(x)` that returns `NA` if `x` is all
#' `NA` or an empty set. Although the sum of an empty is zero, this causes
#' issues in spawn index calculations.
#'
#' @param x A numeric vector.
#' @template param-omit_na
#' @return A numeric vector.
#' @note The sum of an empty set is zero, by definition.
#' @seealso \code{\link{sum}}
#' @family utility functions
#' @export
#' @examples
#' sum(NA, na.rm = TRUE)
#' sum_na(NA)
sum_na <- function(x, omit_na = TRUE) {
  # If all NA or empty
  if (all(is.na(x)) || length(x) == 0) {
    # NA
    res <- NA
  } else { # End if all NA or empty, otherwise
    # Sum
    res <- sum(x, na.rm = omit_na)
  } # End if otherwise
  # Return the result
  return(res)
} # End sum_na function

#' Calculate the mean.
#'
#' Calculate the mean; an alternative to `mean(x)` that returns `NA` if `x` is
#' all `NA` or an empty set.
#'
#' @param x A numeric vector.
#' @template param-omit_na
#' @return A numeric vector.
#' @seealso \code{\link{mean}}
#' @family utility functions
#' @export
#' @examples
#' mean(NA, na.rm = TRUE)
#' mean_na(NA)
mean_na <- function(x, omit_na = TRUE) {
  # If all NA or empty
  if (all(is.na(x)) || length(x) == 0) {
    # NA
    res <- NA
  } else { # End if all NA or empty, otherwise
    # Mean
    res <- mean(x, na.rm = omit_na)
  } # End if otherwise
  # Return the result
  return(res)
} # End mean_na function

#' Calculate the weighted mean.
#'
#' Calculate the weighted mean; an alternative to `weighted.mean(x)` that
#' returns `NA` if `x` is all `NA` or an empty set.
#'
#' @param x A numeric vector of values whose weighted mean is to be computed.
#' @param w A numeric vector of weights of the same length as `x`.
#' @template param-omit_na
#' @importFrom stats weighted.mean
#' @return A numeric vector.
#' @seealso \code{\link{weighted.mean}}
#' @family utility functions
#' @export
#' @examples
#' weighted.mean(NA, na.rm = TRUE)
#' wt_mean_na(NA, w = 1)
wt_mean_na <- function(x, w, omit_na = TRUE) {
  # If all NA or empty
  if (all(is.na(x)) || length(x) == 0) {
    # NA
    res <- NA
  } else { # End if all NA or empty, otherwise
    # Weighted mean
    res <- weighted.mean(x, w, na.rm = omit_na)
  } # End if otherwise
  # Return the result
  return(res)
} # End mean_na function

#' Determine unique values.
#'
#' Determine unique values; an alternative to `unique(x)` that returns `NA` if
#' `x` is all `NA` or an empty set.
#'
#' @param x A vector.
#' @return An object of the same type as `x`.
#' @seealso \code{\link{unique}}
#' @family utility functions
#' @export
#' @examples
#' unique(na.omit(NA))
#' unique_na(NA)
unique_na <- function(x) {
  # If all NA or empty
  if (all(is.na(x)) || length(x) == 0) {
    # NA
    res <- NA
  } else { # End if all NA or empty, otherwise
    # Unique
    res <- unique(x)
  } # End if otherwise
  # Return the result
  return(res)
} # End unique_na function
