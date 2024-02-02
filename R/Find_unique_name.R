#' find unique filename
#'
#' This function saves a given dataframe to a file with a specified base filename.
#' If a file with the base filename already exists, the function appends a numeric
#' suffix to create a unique filename. The function creates the file in the specified
#' directory and ensures that no existing file is overwritten.
#'
#' @param base_filename A character string indicating the base name of the file.
#'                      The function will append a numeric suffix to this base name
#'                      if a file with the same name already exists.
#' @param directory A character string specifying the directory where the file
#'                  will be saved. Defaults to the current working directory.
#' @return A character string containing the path of the file where the data
#'         was saved. This will be the original base filename or a modified
#'         version of it if the original name was already in use.
#' @examples
#' # Example usage:
#' temp_data <- head(mtcars)
#' saved_file_path <- find_unique_filename(temp_data, "mtcars_data.csv")
#' @export
#'
#' @importFrom utils write.csv
find_unique_filename <- function(candidate_filename, directory = getwd()) {
  # Check if candidate_filename is valid
  if(nchar(candidate_filename)==0) {
    stop("candidate_filename must be a non-empty character string.")
  }

  # Check for invalid filename characters
  if (grepl("[\\\\:*?\"<>|]", candidate_filename)) {
    stop("candidate_filename contains invalid characters for a file name.")
  }

  # Ensure the directory ends with a slash
  if (substring(directory, nchar(directory), nchar(directory)) != "/") {
    directory <- paste0(directory, "/")
  }

  # Full path for the candidate file
  full_path <- paste0(directory, candidate_filename)

  # Initialize the counter
  counter <- 1

  # Split the filename and extension
  file_parts <- strsplit(candidate_filename, "\\.", fixed = TRUE)[[1]]
  base_name <- file_parts[1]
  extension <- ifelse(length(file_parts) > 1, paste0(".", file_parts[2]), "")

  # Check if the file exists, and modify the filename until it's unique
  while (file.exists(full_path)) {
    # Create a new filename with a counter
    new_name <- paste0(base_name, "_", counter, extension)
    full_path <- paste0(directory, new_name)
    counter <- counter + 1
  }

  # Return the unique filename
  return(full_path)
}
