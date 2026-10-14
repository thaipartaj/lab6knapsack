#' Solve the Knapsack Problem Using Dynamic Programming
#'
#' Finds the selection of items with the greatest total value
#' without exceeding the maximum total weight.
#'
#' @param x A data frame with exactly two numeric columns:
#' w (weights) and v (values). All entries must be positive
#' and finite. Weights must be whole numbers.
#' @param W A single non-negative, finite whole number giving
#' the maximum total weight.
#'
#' @details
#' Each item can be selected at most once.
#' The algorithm fills a table and then works backwards
#' to identify the selected items.
#' Running time and memory usage are O(nW), where n is
#' the number of items.
#' Values are not rounded during optimization.
#' Only the returned total value is rounded.
#'
#' @return A named list containing:
#' \describe{
#' \item{value}{The maximum total value, rounded to
#' the nearest whole number.}
#' \item{elements}{The row indices of the selected items in x.
#' An empty integer vector if no items are selected.}
#' }
#'
#' @export
#'
#' @examples
#' items <- data.frame(w = c(2, 3, 4), v = c(3, 4, 5))
#' knapsack_dynamic(items, W = 5)
knapsack_dynamic <- function(x, W) {
  # Stoppa funktionen om något av följande krav inte uppfylls.
  stopifnot(
    is.data.frame(x), # x must be a df
    ncol(x) == 2, # x must have exactly 2 columns
    setequal(names(x), c("w", "v")), # colnames must be v and w
    is.numeric(x$w), # weights must be numeric
    is.numeric(x$v),
    all(is.finite(x$w)), # values are not allowed to be NA or infinity
    all(is.finite(x$v)),
    all(x$w > 0), # All weights must be positive
    all(x$v > 0),
    all(x$w == floor(x$w)), # weights must be integers
    is.numeric(W), # the max weight must be numeric
    length(W) == 1, # Only one max weight is allowed
    is.finite(W), # Max weight cannot be NA or infinity
    W >= 0, # Max weight cannot be negative
    W == floor(W) # Max weight must be an integer
  )
  # Every row in x is an object.
  antal_foremal <- nrow(x)
  # Create a table with a row for every number of objects and a column for every maxweight.
  # Must add +1 since the first row is for 0 objects and the first col is for max weight 0.
  # Fill entire table with NAs and then fill the table until it's completed.
  tabell <- matrix(NA, antal_foremal + 1, W + 1)
  # First row should be zeros
  tabell[1, ] <- 0
  # First col should be zeros
  tabell[, 1] <- 0
  # THE FOLLOWING LOOP DOES:
  # For the first object we check weight limits from 1 to W. Then do the same for
  # object 2 and so on...
  # For each limit we calculate the best value without the current object and with it included
  # if it fits. Then we save the higher value in the table
  # Loop through each object from 1,..,n
  for (foremal in seq_len(antal_foremal)) {
    # For every object we will look at the max weights 1,...,W
    for (kapacitet in seq_len(W)) {
      # Start with the best value without the object
      utan <- tabell[foremal, kapacitet + 1]
      # Start with 0 for the alternative of including the object, but the calculations for including
      # it are in the if sats
      med <- 0
      # Check if the object's weight fits within the max weight
      if (x$w[foremal] <= kapacitet) {
        # Add the object's value to the best value from earlier objects that can fit
        # in the remaining capacity.
        med <- x$v[foremal] +
          tabell[foremal, kapacitet - x$w[foremal] + 1]
      }
      # So if the object fits and gives a higher value we save the value otherwise
      # keep the best value without the object. That is max just picks the higher value
      # knowing from ABOVE that med is only calculated if the object fits hehe
      tabell[foremal + 1, kapacitet + 1] <- max(utan, med)
    }
  }
  # Need an empty vector to include chosen objects' id nummer
  valda <- integer(0)
  # Start with the backpack's whole max weight
  kapacitet <- W
  # Loop through all objects reversed that is n,...,1
  for (foremal in rev(seq_len(antal_foremal))) {
    # Now we compare including and excluding this object; if the value is bigger with it
    # then we should have that object.
    if (tabell[foremal + 1, kapacitet + 1] >
        tabell[foremal, kapacitet + 1]) {
      # Then we save the object's id in the defined vector
      valda <- c(foremal, valda)
      # So the object takes up space, so decrease the capacity before searching for other
      # objects.
      kapacitet <- kapacitet - x$w[foremal]
    }
  }
  # In a list we save the best value for the problem here. And then we return the best rounded value
  # and the objects' ids!
  list(
    value = round(tabell[antal_foremal + 1, W + 1]),
    elements = valda
  )
}
