#' Solve the Knapsack Problem by Brute Force
#'
#' Examines every possible selection of items and returns the feasible selection with the greatest total value.
#'
#' @param x A data frame with exactly two numeric columns: w (weights) and v (values). All entries must be positive and finite.
#'
#' @param W A single positive, finite number giving the maximum total weight.
#'
#' @details This implementation supports at most 31 items because it
#'   uses non-negative R integers with intToBits(). Brute-force search
#'   is intended for small inputs: it examines 2^n selections.
#'   If several selections have the same maximum value, the first one
#'   encountered is returned.
#'
#' @return A named list containing:
#'   \describe{
#'     \item{value}{The total value of the selected items, without rounding.}
#'     \item{elements}{The row indices of the selected items in x.}
#'   }
#'
#' @export
#'
#' @examples
#' items <- data.frame(w = c(2, 3, 4), v = c(3, 4, 5))
#' brute_force_knapsack(items, W = 5)

brute_force_knapsack <- function(x, W) {

  # Check the inputs
  stopifnot(
    "x must be a data frame." = is.data.frame(x),
    "x must contain exactly the columns w and v." = ncol(x) == 2L && all(c("w", "v") %in% names(x)),

    "w and v must be numeric vectors." = is.numeric(x$w) && is.null(dim(x$w)) && is.numeric(x$v) && is.null(dim(x$v)),
    "All weights must be positive and finite." = all(is.finite(x$w)) && all(x$w > 0),
    "All values must be positive and finite." = all(is.finite(x$v)) && all(x$v > 0),
    "W must be one positive, finite number." = is.numeric(W) && length(W) == 1L && is.finite(W) && W > 0
  )

  n <- nrow(x)

  stopifnot("This implementation supports at most 31 items." = n <= 31L)

  # Start with the empty selection
  # In cases where no item fits, the function returns value 0 and an empty selection
  best_value <- 0
  best_elements <- integer(0)

  # Check every possible selection.
  # There are 2^n possible selections of n items.
  # Label them from 0 (take nothing) to 2^n - 1 (take everything).
  # Each label's first n binary bits form the selection checklist.
  weights <- x$w
  values <- x$v
  for (selection in 0:(2^n - 1)) {

    # Create a checklist with one bit for each item.
    bits <- as.integer(intToBits(selection))[seq_len(n)]

    # Find the indices of the items marked with a 1.
    elements <- which(bits == 1L)

    total_weight <- sum(weights[elements])

    # Only consider selections that fit.
    if (total_weight <= W) {
      total_value <- sum(values[elements])

      # Remember this selection if it improves the best value.
      if (total_value > best_value) {
        best_value <- total_value
        best_elements <- elements
      }
    }
  }

  # Return the best selection found.
  return(list(
    value = best_value,
    elements = best_elements
  ))
}
