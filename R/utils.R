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
