# Function to plot singular values for each matrix F
#' Plot Singular Values of F Coefficients
#'
#' This function takes a vector of F coefficients and plots the singular values
#' for each corresponding diagonal matrix. The function handles various cases
#' including matrices with NA values, empty matrices, and matrices with only zeroes.
#'
#' @param F_coefficients A numeric vector representing F coefficients used to
#' create diagonal matrices. NA values are omitted in the analysis.
#'
#' @details The function iterates over the cleaned F coefficients, creating
#' diagonal matrices for each coefficient. It handles cases where the matrix
#' is empty, contains only a single value, or contains only zero values.
#' For valid matrices, it calculates the singular values using Singular Value
#' Decomposition (SVD) and plots these values.
#'
#' The function utilizes `ggplot2` for plotting. Ensure that `ggplot2` is
#' installed and loaded in your R session.
#'
#' @examples
#' # Example usage
#' F_coeffs <- c(1, 2, 3, NA, 4)
#' plot_singular_values(F_coeffs)
#'
#' @importFrom ggplot2 ggplot geom_line labs theme_minimal
plot_singular_values <- function(F_coefficients) {
  # Function implementation

    # Remove NA values
  F_coefficients_clean <- na.omit(F_coefficients)

  # Plot singular values for each matrix F
  for (i in seq_along(F_coefficients_clean)) {
    F_matrix <- diag(F_coefficients_clean[i],
                     nrow = length(F_coefficients_clean))

        # Remove Second Level NA values
    F_matrix_clean <- na.omit(F_matrix)



    # Check if matrix is empty
    if(sum(dim(F_matrix_clean) == 0)) {
      print("Matrix is empty. Nothing new in the front.")
                              } else if(sum(dim(F_matrix_clean) == 1))
                                {
    print("There's not even an entire matrix, just a value of:",
          F_matrix_clean) } else if (sum(F_matrix_clean)==0){
        print("We got just singular values zeroes")  } else {
    # Calculate singular values
    singular_values <- svd(F_matrix_clean)$d
    plot_title <- paste("Singular Values for F Matrix", i)

    # Plotting singular values
    ggplot(data.frame(Singular_Values = singular_values),
           aes(x = seq_along(singular_values), y = Singular_Values)) +
      geom_line() +
      labs(title = plot_title,
           x = "Index",
           y = "Singular Value") +
      theme_minimal()                }}
}
