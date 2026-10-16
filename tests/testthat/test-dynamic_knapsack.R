# Using the settings presented in the lab instructions
suppressWarnings(RNGversion(min(as.character(getRversion()), "3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")

# Making 2000 objects with random generated weights and values
n <- 2000
knapsack_objects <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)

# Checking that the function runs without output, returning the correct names
test_that("Correct object is returned", {
  expect_silent(dpk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))
  expect_named(dpk, c("value", "elements"))
})

# Checking that wrong input gives an error
test_that("Function rejects erroneous input.", {
  expect_error(knapsack_dynamic("hej", 3500))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8,], W = -3500))
})

# Check the results for different numbers of objects and weight limits
test_that("Function returns correct results.", {

  # Testing 8 objects with max weight 3500
  dpk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(dpk$value), 16770)
  expect_true(all(round(dpk$elements) %in% c(5, 8)))

  # Testing 12 objects with max weight 3500
  dpk <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(dpk$value), 16770)
  expect_true(all(round(dpk$elements) %in% c(5, 8)))

  # Testing 8 objects with max weight 2000
  dpk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(dpk$value), 15428)
  expect_true(all(round(dpk$elements) %in% c(3, 8)))

  # Testing 12 objects with max weight 2000
  dpk <- knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(dpk$value), 15428)
  expect_true(all(round(dpk$elements) %in% c(3, 8)))

  # Measuring the time for 16 objects, checking that sys time cannot be negative
  st <- system.time(dpk <- knapsack_dynamic(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] >= 0.00)

})
