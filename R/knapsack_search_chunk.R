# Internal helper: search one consecutive range of combination numbers.
# This function is used by the workers and does not need @export.
knapsack_search_chunk <- function(bounds, weights, values, W) {

  # Each weight corresponds to one item from the dataset.
  n <- length(weights)

  # Start with the empty selection.
  # This also handles cases where no item fits as the main function.
  best_value <- 0
  best_elements <- integer(0)

  # bounds contains the first and last combination numbers for this task.
  for (selection in seq.int(bounds[1L], bounds[2L])) {

    # Convert the combination number into an item-selection checklist.
    # The first n bits correspond to the n items: 1 means include the item and 0 means exclude it.
    bits <- as.integer(intToBits(selection))[seq_len(n)]

    # Obtain the original row indices of the selected items.
    elements <- which(bits == 1L)

    # Calculate the total weight of this selection.
    total_weight <- sum(weights[elements])

    # Only calculate its value if the selection fits.
    if (total_weight <= W) {
      total_value <- sum(values[elements])

      # Remember the selection if it improves the current best value.
      # Using > keeps the first combination when values are tied.
      if (total_value > best_value) {
        best_value <- total_value
        best_elements <- elements
      }
    }
  }

  # Return this task's best feasible selection.
  return(list(value = best_value,
              elements = best_elements)
  )
  }
