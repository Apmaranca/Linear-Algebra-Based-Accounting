# 1. Test for Invalid File Names
# This test checks the function's behavior when provided with an invalid
# file name. An invalid file name could be a name with forbidden characters
# or an empty string.

test_that("Function handles invalid file names", {
  temp_dir <- tempdir()
  invalid_filenames <- c("invalid/name.csv", "", NULL)

  for (filename in invalid_filenames) {
    expect_error(find_unique_filename(mtcars, filename, temp_dir))
  }
})

# 2. Test for Badly Formatted Directories
# This test verifies how the function handles directories that are badly
# formatted or don't exist.

test_that("Function handles badly formatted directories", {
  bad_directory <- "this/directory/does/not/exist/"
  base_filename <- "test_file.csv"

  expect_error(find_unique_filename(mtcars, base_filename, bad_directory))
})


# 3. Test for Null File Name
# This test checks the function's behavior when the file name provided
# is NULL.

test_that("Function handles null file name", {
  temp_dir <- tempdir()

  expect_error(find_unique_filename(mtcars, NULL, temp_dir))
})



# 4. Test for Handling Invalid Input
# This test checks the function's behavior with invalid input, such as a
# null or empty filename, which should result in an error or exception being
# thrown.

test_that("Function handles invalid input", {
  temp_dir <- tempdir()
  invalid_filename <- NULL  # or use an empty string ""

  expect_error(find_unique_filename(mtcars, invalid_filename, temp_dir))
})
