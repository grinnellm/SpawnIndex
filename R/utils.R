#' Paste things together nicely.
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
#' paste_nicely(x = letters[1:5])
paste_nicely <- function(x,
                         int_char = ", ",
                         n_char = "and ",
                         do_quotes = FALSE,
                         quotes = c("`", "`")) {
  # Check input: characters
  if (!is.character(int_char) | !is.character(n_char) | !is.character(quotes)) {
    stop("`int_char`, `n_char`, and `quotes` must be character.", call. = FALSE)
  }
  # Check input: logical
  if (!is.logical(do_quotes)) {
    stop("`do_quotes` must be logical.", call. = FALSE)
  }
  # Check for two values in quotes
  if (do_quotes & length(quotes) != 2) stop("`quotes` must have two values.")
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

#' Minimum

#' Calculate minimum if there are non-NA values, return NA if all values are NA

#' @param x Object.
#' @param omit_na Omit NAs.
#' @return Numeric.
#' @seealso \code{\link{max_na}}, \code{\link{sum_na}}, \code{\link{mean_na}},
#'   \code{\link{wt_mean_na}}, \code{\link{roll_mean_na}},
#'   \code{\link{unique_na}}
#' @family utility functions
#' @export
min_na <- function(x, omit_na = TRUE) {
  # An alternate version to min(x, na.rm=TRUE), which returns 0 if x is all NA.
  # This version retuns NA if x is all NA, otherwise it returns the minimum.
  # If all NA, NA; otherwise, minimum
  ifelse(all(is.na(x)),
    res <- NA,
    res <- min(x, na.rm = omit_na)
  )
  # Return the result
  return(res)
} # End min_na function

#' Maximum

#' Calculate maximum if there are non-NA values, return NA if all values are NA

#' @param x Object.
#' @param omit_na Omit NAs.
#' @return Numeric.
#' @seealso \code{\link{min_na}}, \code{\link{sum_na}}, \code{\link{mean_na}},
#'   \code{\link{wt_mean_na}}, \code{\link{roll_mean_na}},
#'   \code{\link{unique_na}}
#' @family utility functions
#' @export
max_na <- function(x, omit_na = TRUE) {
  # An alternate version to max(x, na.rm=TRUE), which returns 0 if x is all NA.
  # This version retuns NA if x is all NA, otherwise it returns the maximum.
  # If all NA, NA; otherwise, maximum
  ifelse(all(is.na(x)),
    res <- NA,
    res <- max(x, na.rm = omit_na)
  )
  # Return the result
  return(res)
} # End max_na function

#' Sum

#' Calculate sum if there are non-NA values, return NA if all values are NA

#' @param x Object.
#' @param omit_na Omit NAs.
#' @return Numeric.
#' @seealso \code{\link{min_na}}, \code{\link{max_na}}, \code{\link{mean_na}},
#'   \code{\link{wt_mean_na}}, \code{\link{roll_mean_na}},
#'   \code{\link{unique_na}}
#' @family utility functions
#' @export
sum_na <- function(x, omit_na = TRUE) {
  # An alternate version to sum(x, na.rm=TRUE), which returns 0 if x is all NA.
  # This version retuns NA if x is all NA, otherwise it returns the sum.
  # If all NA, NA; otherwise, sum
  ifelse(all(is.na(x)),
    res <- NA,
    res <- sum(x, na.rm = omit_na)
  )
  # Return the result
  return(res)
} # End sum_na function

#' Mean

#' Calculate mean if there are non-NA values, return NA if all values are NA

#' @param x Object.
#' @param omit_na Omit NAs.
#' @return Numeric.
#' @seealso \code{\link{min_na}}, \code{\link{max_na}}, \code{\link{sum_na}},
#'   \code{\link{wt_mean_na}}, \code{\link{roll_mean_na}},
#'   \code{\link{unique_na}}
#' @family utility functions
#' @export
mean_na <- function(x, omit_na = TRUE) {
  # An alternate version to mean(x, na.rm=TRUE), which returns 0 if x is all NA.
  # This version retuns NA if x is all NA, otherwise it returns the mean.
  # If all NA, NA; otherwise, mean
  ifelse(all(is.na(x)),
    res <- NA,
    res <- mean(x, na.rm = omit_na)
  )
  # Return the result
  return(res)
} # End mean_na function

#' Weighted mean

#' Calculate weighted mean if there are non-NA values, return NA if all values
#' are NA

#' @param x Object.
#' @param w Weights.
#' @param omit_na Omit NAs.
#' @importFrom stats weighted.mean
#' @return Numeric.
#' @seealso \code{\link{min_na}}, \code{\link{max_na}}, \code{\link{sum_na}},
#'   \code{\link{mean_na}}, \code{\link{roll_mean_na}}, \code{\link{unique_na}}
#' @family utility functions
#' @export
wt_mean_na <- function(x, w, omit_na = TRUE) {
  # An alternate version to weighted.mean(x, w, na.rm=TRUE), which returns 0 if
  # x is all NA. This version retuns NA if x is all NA, otherwise it returns the
  # weighted mean.
  # If all NA, NA; otherwise, weighted mean
  ifelse(all(is.na(x)),
    res <- NA,
    res <- weighted.mean(x, w, na.rm = omit_na)
  )
  # Return the result
  return(res)
} # End mean_na function

#' Rolling mean (window)

#' Fill in NA values with rolling mean of previous values

#' @param dat Object.
#' @param n Window size.
#' @param omit_na Omit NAs.
#' @return Numeric.
#' @seealso \code{\link{min_na}}, \code{\link{max_na}}, \code{\link{sum_na}},
#'   \code{\link{mean_na}}, \code{\link{wt_mean_na}}, \code{\link{unique_na}}
#' @family utility functions
#' @export
roll_mean_na <- function(dat, n, omit_na = TRUE) {
  # Update the NAs in a vector with the mean of the previous values. The number
  # of values used can be less than n for NAs near the start of the vector, and
  # will be a maximum of n for values further along the vector. The value will
  # remain NA if no non-NA values are available.
  # Loop over observations starting with the second observation
  for (i in 2:length(dat)) {
    # If the value is NA
    if (is.na(dat[i])) {
      # Get window for current index: up to n previous values
      muWindow <- (i - min(n, i - 1)):(i - 1)
      # Calculate the mean of the values in the rolling window
      dat[i] <- mean_na(dat[muWindow], omit_na = omit_na)
    } # End if value is NA
  } # End i loop over observations
  # Return the observations with NAs (mostly) replaced by the rolling mean
  return(dat)
} # End roll_mean_na function

#' Unique

#' Calculate unique if there are non-NA values, return NA if all values are NA

#' @param x Object.
#' @return Numeric.
#' @seealso \code{\link{min_na}}, \code{\link{max_na}}, \code{\link{sum_na}},
#'   \code{\link{mean_na}}, \code{\link{wt_mean_na}}, \code{\link{roll_mean_na}}
#' @family utility functions
#' @export
unique_na <- function(x) {
  # An alternate version to unique, which fails sometimes if there are no
  # values. This version retuns NA if x is all NA, otherwise it returns the
  # unique values.
  # If all NA, NA; otherwise, unique
  ifelse(all(is.na(x)),
    res <- NA,
    res <- unique(x)
  )
  # Return the result
  return(res)
} # End unique_na function
