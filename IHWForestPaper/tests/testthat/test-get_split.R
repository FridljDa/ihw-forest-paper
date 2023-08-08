library(testthat)
library(dplyr)

# Assuming you've already defined the get_split function as before...

test_that("get_split function works correctly", {
  # Create a mock dataframe for testing
  df <- data.frame(a = 1:100, b = rnorm(100))
  
  num_splits <- 5
  
  # Get the split using the function
  split_data <- get_split(df, num_splits, split_index = 3)
  
  # Check that the size of the split is as expected
  expected_size <- ceiling(nrow(df) / num_splits)
  expect_equal(nrow(split_data), expected_size)
  
  # Check that the split data is a subset of the original data
  expect_true(all(split_data$a %in% df$a))
  expect_true(all(split_data$b %in% df$b))
})

# Assuming you've already defined the get_split function...

test_that("get_split function reconstructs the entire dataframe when iterating over splits", {
  # Create a mock dataframe for testing
  df <- data.frame(a = 1:100, b = rnorm(100))
  
  num_splits <- 5
  all_splits <- lapply(1:num_splits, function(i) get_split(df, num_splits, i))
  
  # Combine all splits back together
  reconstructed_df <- bind_rows(all_splits)
  
  # Sort both original and reconstructed data.frames for comparison
  df_sorted <- df %>% arrange(a, b)
  reconstructed_df_sorted <- reconstructed_df %>% arrange(a, b)
  
  # Check that they're the same
  expect_equal(df_sorted, reconstructed_df_sorted)
})
