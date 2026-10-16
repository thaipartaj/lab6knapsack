#' This code solves the knapsack problem using the dynamic programming approach + with a fun recursie way
#'
#' It finds objects with the highest max value without going over
#' maximum weight chosen. Each object can only be selected once
#' The function uses recursion and saves results in a table which is filled by the recursie recursion function
#'
#' @param x A data frame with two numeric columns: \code{w}, containing
#'   positive integer weights, and \code{v}, containing positive values.
#'   Both columns must contain only finite numbers.
#' @param W A single finite, non-negative integer giving the maximum weight.
#'
#' @return A list is returned with:
#' \describe{
#'   \item{value}{The highest total value}
#'   \item{elements}{The row numbers of the selected objects in \code{x}.}
#' }
#'
#' @examples
#' objects <- data.frame(
#'   w = c(2, 3, 4),
#'   v = c(10, 20, 25)
#' )
#' knapsack_dynamic(objects, W = 5)
#'
#' @export
knapsack_dynamic <- function(x, W) {

  # Being by checking if the inputs are correct
  stopifnot(
    is.data.frame(x),
    ncol(x) == 2,
    setequal(names(x), c("w", "v")),
    is.numeric(x$w),
    is.numeric(x$v),
    all(is.finite(x$w)),
    all(is.finite(x$v)),
    all(x$w > 0),
    all(x$v > 0),
    all(x$w == floor(x$w)),
    is.numeric(W),
    length(W) == 1,
    is.finite(W),
    W >= 0,
    W == floor(W)
  )

  # Defining the amount of objects
  antal_foremal <- nrow(x)

  # Creating the matrix to save values in, +1 is for the 0 row and column
  tabell <- matrix(NA, antal_foremal + 1, W + 1)



  # Now recursive function is used ti find the best value using the first objects with a secified capacity.
  KS <- function(foremal, kapacitet) {

    # This just stops the function if it already has been saved
    if (!is.na(tabell[foremal + 1, kapacitet + 1])) {
      return(tabell[foremal + 1, kapacitet + 1])
    }

    # This checks if we have run out of objects or we've reached max space in the knapsack
    if (foremal == 0 || kapacitet == 0) {
      result <- 0

    # This codes checks that the objects ins't too heavy for the knap
    } else if (x$w[foremal] > kapacitet) {
      result <- KS(foremal - 1, kapacitet)

    } else {

      # This just says if it fits in the knapsack we should compare two alterantives
      # utan will then be the best value if we leave the current object
      utan <- KS(foremal - 1, kapacitet)

      # This will be the best value including the current object and we add its value to the best value for the remaining capacity
      med <- x$v[foremal] +
        KS(foremal - 1, kapacitet - x$w[foremal])

      # We always want to save the highest objects of the two alternatives.
      result <- max(utan, med)
    }

    # Save the answer in the table outside KS
    tabell[foremal + 1, kapacitet + 1] <<- result

    return(result)
  }

  # Solve the full problem and save the answers in the table (matrix)
  KS(antal_foremal, W)

  # We create a vector to save the chosen objects in
  valda <- integer(0)

  # We start with the backpacks max weight
  kapacitet <- W

  # Then we can loop through all objects reversed that just is from the last to the first
  for (foremal in rev(seq_len(antal_foremal))) {

    # If the value is higher with this object include it
    if (KS(foremal, kapacitet) >
        KS(foremal - 1, kapacitet)) {

      # Then we save the row number of this object
      valda <- c(foremal, valda)

      # Then we must decrease the remaning capacity by the objects weight
      kapacitet <- kapacitet - x$w[foremal]
    }
  }

  # Now we can return the object and best chosen objects!!
  list(
    value = tabell[antal_foremal + 1, W + 1],
    elements = valda
  )
}
