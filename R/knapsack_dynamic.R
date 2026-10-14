knapsack_dynamic <- function(x, W) {

  # Check the inputs
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

  # Every row in x is an object
  antal_foremal <- nrow(x)

  # Create a table for saving answers
  # +1 gives space for 0 objects and capacity 0
  tabell <- matrix(NA, antal_foremal + 1, W + 1)

  # Find the best value using the first foremal objects
  # with the given kapacitet
  KS <- function(foremal, kapacitet) {

    # If this answer is already saved, return it directly
    if (!is.na(tabell[foremal + 1, kapacitet + 1])) {
      return(tabell[foremal + 1, kapacitet + 1])
    }

    # Check if no objects are left or no space is left then the best value is 0
    if (foremal == 0 || kapacitet == 0) {
      result <- 0

      # If the object is too heavy, skip it
    } else if (x$w[foremal] > kapacitet) {
      result <- KS(foremal - 1, kapacitet)

    } else {

      # This just says if it fits in the knapsack we should compare two alterantives
      # utan will then be the best value if we leave the current object,
      utan <- KS(foremal - 1, kapacitet)

      # Best value including the current object
      # Add its value to the best value for the remaining capacity
      med <- x$v[foremal] +
        KS(foremal - 1, kapacitet - x$w[foremal])

      # Keep the higher value
      result <- max(utan, med)
    }

    # Save the answer in the table outside KS
    tabell[foremal + 1, kapacitet + 1] <<- result

    return(result)
  }

  # Solve the full problem and save the answers in tabell
  KS(antal_foremal, W)

  # Need an empty vector for the chosen objects
  valda <- integer(0)

  # Start with the backpack's whole max weight
  kapacitet <- W

  # Loop through all objects reversed, from the last to the first
  for (foremal in rev(seq_len(antal_foremal))) {

    # If the value is higher with this object available,
    # we need to include it
    if (KS(foremal, kapacitet) >
        KS(foremal - 1, kapacitet)) {

      # Save the object's row number
      valda <- c(foremal, valda)

      # Decrease the remaining capacity by the object's weight
      kapacitet <- kapacitet - x$w[foremal]
    }
  }

  # Return the rounded best value and the chosen objects
  list(
    value = round(tabell[antal_foremal + 1, W + 1]),
    elements = valda
  )
}
