#' Solve the Knapsack Problem with a Greedy Heuristic
#'
#' Ranks items by value per unit of weight and takes items in that order
#' until the next item does not fit. Returns the better of this selection
#' and the most valuable single item that fits.
#'
#' @param x A data frame with exactly two numeric columns: w (weights) and v (values). All entries must be positive and finite.
#'
#' @param W A single positive, finite number giving the maximum total weight.
#'
#' @details Items heavier than W are excluded before selection.
#'   The method is approximate and has the usual O(n log n) sorting cost.
#'   Comparing with the best feasible single item gives the standard
#'   one-half approximation guarantee for the 0-1 knapsack problem.
#'   In other words, at least 50% of the best possible total value across all valid combinations.
#'
#' @return A named list containing:
#'   \describe{
#'     \item{value}{The total value of the selected items, without rounding.}
#'     \item{elements}{The original row indices of the selected items in x.}
#'   }
#'
#' @export
#'
#' @examples
#' items <- data.frame(w = c(10, 20, 30), v = c(60, 100, 120))
#' greedy_knapsack(items, W = 50)

greedy_knapsack <- function(x, W) {

  # Check the inputs.
  stopifnot(
    "x must be a data frame." = is.data.frame(x),
    "x must contain exactly the columns w and v." = ncol(x) == 2L && all(c("w", "v") %in% names(x)),

    "w and v must be numeric vectors." = is.numeric(x$w) && is.null(dim(x$w)) && is.numeric(x$v) && is.null(dim(x$v)),

    "All weights must be positive and finite." = all(is.finite(x$w)) && all(x$w > 0),

    "All values must be positive and finite." = all(is.finite(x$v)) && all(x$v > 0),

    "W must be one positive, finite number." = is.numeric(W) && length(W) == 1L && is.finite(W) && W > 0
    )

  # Keep the original indices of items that can fit individually.
  eligible <- which(x$w <= W)

  if (length(eligible) == 0L) return(list(value = 0, elements = integer(0)))

  # Rank eligible items by decreasing value per unit of weight.
  ratio <- x$v[eligible] / x$w[eligible]
  item_order <- eligible[order(ratio, decreasing = TRUE)]

  # Take the initial items whose cumulative weight fits.
  cumulative_weight <- cumsum(x$w[item_order])
  elements <- item_order[cumulative_weight <= W]
  total_value <- sum(x$v[elements])

  # Use the best single item instead if it has a greater value.
  best_single <- eligible[which.max(x$v[eligible])]

  if (x$v[best_single] > total_value) {
    elements <- best_single
    total_value <- x$v[best_single]
  }

  # Return the selected value and original row indices.
  return(list(value = total_value, elements = elements))
}
