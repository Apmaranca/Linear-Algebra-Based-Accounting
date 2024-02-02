# 1. Test for Creating File with Default Data
# This test checks whether the function correctly creates a new file with
# default data if the specified file does not exist. This ensures that the
# function behaves as expected in the absence of an existing file.

test_that("Function creates file with default data if it does not exist", {
  temp_file_path <- tempfile(fileext = ".dat")

  # Call the function with a non-existent file path
  result <- read_or_create_juice_data(temp_file_path)

  # Check if the file was created
  expect_true(file.exists(temp_file_path))

  # Check if the default data is correctly initialized
  default_Rb <- c(1, 0, 1, 0, 0, 1, 1, 1)
  default_Rs <- c(0, 1, 0, 1, 1, 0, 0, 0)
  expect_equal(result$Rb, default_Rb)
  expect_equal(result$Rs, default_Rs)

  # Clean up
  unlink(temp_file_path)
})

#2. Test for Correctly Reading Existing Data
# This test verifies that the function can correctly read and return data
# from an existing file. This is crucial for ensuring data integrity and
# proper functionality in scenarios where the file already exists.

test_that("Function correctly reads existing data", {
  temp_file_path <- tempfile(fileext = ".dat")
  predefined_data <- data.frame(Rb = 1:9, Rs = 9:1)
  write.csv(predefined_data, temp_file_path, row.names = FALSE)

  # Call the function with an existing file path
  result <- read_or_create_juice_data(temp_file_path)

  # Check if the data is read correctly
  expect_equal(result$Rb, 1:9)
  expect_equal(result$Rs, 9:1)

  # Clean up
  unlink(temp_file_path)
})

# 3. Test for Error Handling with Improperly Formatted Data
# This test ensures that the function properly handles cases where the
# existing file is wrongly formatted (e.g., missing required columns). It's
# essential for robust error handling.

test_that("Function handles improperly formatted data", {
  temp_file_path <- tempfile(fileext = ".dat")
  wrong_data <- data.frame(WrongColumn1 = 1:9, WrongColumn2 = 9:1)
  write.csv(wrong_data, temp_file_path, row.names = FALSE)

  # Call the function and expect an error
  expect_error(read_or_create_juice_data(temp_file_path))

  # Clean up
  unlink(temp_file_path)
})


