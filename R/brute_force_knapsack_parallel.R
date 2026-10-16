#' Solve the Knapsack Problem by Parallel Brute Force
#'
#' Splits the combination numbers into ranges, searches the ranges using
#' separate R workers, and returns the best feasible selection.
#'
#' @param x A data frame with exactly two numeric columns, w and v. All weights and values must be positive and finite.
#' @param W A single positive, finite number giving the maximum total weight.
#' @param cores Number of workers. Must be a positive whole number.
#'   NULL uses parallel::detectCores(), with a fallback to one worker if the number of cores cannot be detected.
#'
#' @details At most 31 items are supported because the search uses
#'   non-negative R integers with intToBits(). All 2^n combinations are
#'   examined. Parallel execution distributes the search across workers;
#'   starting and stopping those workers introduces overhead.
#'   Ties are resolved using the first combination in ascending numeric order.
#'   PSOCK workers support Windows, macOS and Linux.
#'
#' @return A named list with value (the unrounded total value) and elements (the selected row indices in x).
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' items <- data.frame(w = c(1, 2, 3), v = c(2, 4, 4))
#' brute_force_knapsack_parallel(items, W = 3, cores = 2)
#' }
brute_force_knapsack_parallel <- function(x, W, cores = NULL) {

  # Use the same input requirements as the existing serial function.
  stopifnot("x must be a data frame." = is.data.frame(x),
            "x must contain exactly the columns w and v." = ncol(x) == 2L && all(c("w", "v") %in% names(x)),

            "w and v must be numeric vectors." = is.numeric(x$w) && is.null(dim(x$w)) && is.numeric(x$v) && is.null(dim(x$v)),

            "All weights must be positive and finite." = all(is.finite(x$w)) && all(x$w > 0),
            "All values must be positive and finite." = all(is.finite(x$v)) && all(x$v > 0),
            "W must be one positive, finite number." = is.numeric(W) && length(W) == 1L && is.finite(W) && W > 0)

  n <- nrow(x)

  stopifnot("This implementation supports at most 31 items." = n <= 31L)

  # Retrieve the columns once, as in the improved serial implementation.
  weights <- x$w
  values <- x$v

  # n items give 2^n combinations, numbered/labeled from 0 to 2^n - 1.
  total <- 2^n

  # Detect the worker count only when the user has not supplied cores.
  if (is.null(cores)) {
    cores <- parallel::detectCores()

    # Use one worker if detection returns a missing value.
    if (is.na(cores)) cores <- 1L
  }

  stopifnot("cores must be one positive whole number." = is.numeric(cores) && length(cores) == 1L &&
              is.finite(cores) && cores >= 1 &&
              cores == floor(cores) # Just check that # cores is a whole number
            )

  # Avoid creating more workers than there are combinations.
  cores <- min(cores, total)

  # With one worker, search directly to avoid cluster startup.
  # For zero items, total is 1 and the helper returns the empty selection.
  if (cores == 1L) {
    return(knapsack_search_chunk(bounds = c(0, total - 1),
                                 weights,
                                 values,
                                 W))
    }

  # Calculate approximately equal-sized ranges.
  # Example: total = 8 and cores = 2 gives boundaries c(0, 4, 8).
  # cores + 1 are our separators. So 1 separator at 0, 1 at total and 1 in the middle for the example above.
  # We only store boundaries, rather than generating all combination numbers.
  boundaries <- floor(seq(0, total, length.out = cores + 1))

  # Each chunk stores c(first combination, last combination).
  # Subtracting 1 prevents overlap:
  # boundaries c(0, 4, 8) give chunks c(0, 3) and c(4, 7).
  chunks <- lapply(seq_len(cores), function(i) {c(boundaries[i], boundaries[i + 1L] - 1)})

  # Start separate R worker processes.
  # PSOCK is the cluster approach that also works on Windows for us.
  cl <- parallel::makeCluster(cores, type = "PSOCK")

  # Register cleanup immediately after creating the cluster.
  # Workers stop when this function exits, including after a later error.
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # Each element of X becomes the helper's first argument, bounds.
  # The additional arguments supply the same item data to every task.
  results <- parallel::parLapply(
    cl = cl,
    X = chunks,
    fun = knapsack_search_chunk,
    weights = weights,
    values = values,
    W = W
  )

  # Extract one best value from each task's returned list.
  # which.max() gives the position of the first largest value.
  # parLapply preserves chunk order, so ties match the serial search.
  best <- which.max(vapply(results, function(z) z$value, numeric(1)))

  # Return the complete winning result: both value and selected elements.
  return(results[[best]])
}
