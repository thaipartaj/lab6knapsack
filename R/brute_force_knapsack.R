#' Solve the Knapsack Problem by Brute Force
#'
#' Examines every possible selection of items and returns the feasible
#' selection with the greatest total value. The search can run serially
#' or in parallel.
#'
#' @param x A data frame with exactly two numeric columns: w (weights) and v (values). All entries must be positive and finite.
#' @param W A single positive, finite number giving the maximum total weight.
#' @param parallel A single non-missing logical value. If FALSE, the search runs serially.
#'                 If TRUE, the search uses the internal parallel implementation. Defaults to FALSE.
#' @param cores Requested number of workers when parallel is TRUE.
#'   Must be NULL or a single positive, finite whole number.
#'   NULL uses parallel::detectCores(), with a fallback to one worker if detection fails. Ignored when parallel is FALSE.
#'
#' @details
#' This implementation supports at most 31 items because it uses
#' non-negative R integers with intToBits(). It examines all \eqn{2^n} possible selections, where n is the number of rows in x.
#' Brute-force search is therefore intended for small inputs.
#'
#' In parallel mode, the combination numbers are divided into consecutive
#' ranges. Each worker searches its assigned range, and the best result
#' across all ranges is returned. The implementation uses PSOCK workers,
#' which support Windows, macOS and Linux.
#'
#' The number of workers is capped at the number of combinations.
#' If only one worker is requested or needed, the search runs directly
#' without starting a cluster.
#'
#' Starting workers, transferring data, collecting results and stopping
#' workers introduce overhead. Parallel execution may therefore be slower
#' than serial execution for small inputs.
#'
#' Both modes return the same solution. If several selections have the
#' same maximum value, the first selection in ascending combination-number
#' order is returned.
#'
#' @return A named list containing:
#'   \describe{
#'     \item{value}{The total value of the selected items, without rounding.}
#'     \item{elements}{The row indices of the selected items in x.}
#'   }
#'   If no item fits, value is zero and elements is an empty integer vector.
#'
#' @export
#'
#' @examples
#' items <- data.frame(w = c(2, 3, 4), v = c(3, 4, 5))
#'
#' # Serial search
#' brute_force_knapsack(items, W = 5)
#'
#' \dontrun{
#' # Parallel search using two workers
#' brute_force_knapsack(items, W = 5, parallel = TRUE, cores = 2)
#' }

brute_force_knapsack <- function(x, W, parallel = FALSE, cores = NULL) {

  # Validate the switch before choosing an implementation.
  stopifnot("parallel must be TRUE or FALSE." = is.logical(parallel) && length(parallel) == 1L && !is.na(parallel))

  # Forward parallel requests to the internal parallel implementation.
  if (parallel) {return(brute_force_knapsack_parallel(x, W, cores = cores))}

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
