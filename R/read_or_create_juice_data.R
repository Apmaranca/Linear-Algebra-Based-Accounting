#' Read or Create Juice Data
#'
#' This function reads juice-related data from a specified file. If the file does not exist,
#' it creates the file with default data. The function checks for correct file format
#' and handles errors in file reading.
#'
#' @param file_path The file path for the juice data file. If the file does not exist,
#'        a new file with default data is created at this path.
#'
#' @return A list with two components: 'Rb' and 'Rs'. Each component is a vector
#'         representing specific juice data. If the file exists, data is read from the file;
#'         otherwise, default data is returned.
#'
#' @details If the file at 'file_path' does not exist, the function creates it with default
#'          values for 'Rb' and 'Rs'. If the file exists but is wrongly formatted (i.e.,
#'          missing 'Rb' or 'Rs' columns), the function stops and throws an error.
#'
#' @examples
#' # Example usage with a specific file path
#' result <- read_or_create_juice_data("path/to/juice_data.csv")
#'
#' @importFrom utils read.csv write.csv
#' @importFrom base dir.create
#' @importFrom magrittr %>%
#' @export
read_or_create_juice_data <- function(file_path="extdata/juice_data.csv") {
  # Function implementation

  default_Rb <- c(1, 0, 1, 0, 0, 1, 1, 1)
  default_Rs <- c(0, 1, 0, 1, 1, 0, 0, 0)

  # Check if the file exists
  if (!file.exists(file_path)) {
    # If file does not exist, create default data
    default_data <- data.frame(Rb = default_Rb, Rs = default_Rs)

    # Create the directory if it does not exist
    if (!dir.exists(dirname(file_path))) {
      dir.create(dirname(file_path), recursive = TRUE)
    }

    # Write the default data to a CSV file
    write.csv(default_data, file_path, row.names = FALSE)
    return(list(Rb = default_Rb, Rs = default_Rs))
  } else {
    # If file exists, attempt to read the data from the file
    data <- tryCatch({
      read.csv(file_path)
    }, error = function(e) {
      stop("Error reading the file: ", e$message)
    })

    # Check if the data has the correct format
    if (!all(c("Rb", "Rs") %in% names(data))) {
      stop("Data file is wrongly formatted. Expected columns 'Rb' and 'Rs' not found.")
    }

    # Extract vectors from the data frame
    Rb <- data$Rb
    Rs <- data$Rs
    return(list(Rb = Rb, Rs = Rs))
  }
}


