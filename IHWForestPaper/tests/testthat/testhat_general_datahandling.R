#testthat_createdataframe
library(testthat)

devtools::load_all()
test_that("create_dataframe function works correctly", {
  
  vec1 <- c(1,2,3)
  vec2 <- c(4,5,6)
  vec3 <- c(7,8,9)
  vec_list <- list(vec1, vec2, vec3)
  
  result <- create_dataframe(vec_list)
  
  # The expected result should have 9 rows (3 vectors with 3 values each)
  # and 3 columns (number of vectors)
  expected_rows <- length(vec1) + length(vec2) + length(vec3)
  expected_cols <- length(vec_list)
  
  expect_equal(nrow(result), expected_rows)
  expect_equal(ncol(result), expected_cols)
  
  # Check if the values in each column are correct
  for(i in seq_along(vec_list)) {
    # Check if the first value in each column is the same as the first value of the corresponding vector
    expect_equal(result[1,i], vec_list[[i]][1])
  }
})
