#' Check Matrix Invertibility and Compute Inverse or Pseudoinverse
#'
#' This function checks if a given matrix is invertible. If the matrix is square and invertible,
#' it returns its inverse and determinant. For non-square matrices or matrices close to singular,
#' it returns the pseudoinverse and singular value decomposition. The function also plots the
#' inverse or pseudoinverse matrix.
#'
#' @param mat A numeric matrix for which invertibility is checked.
#' @param epsilon A small numerical value (default is 0.000001) used to determine if the determinant
#'        is close to zero, indicating near-singularity.
#'
#' @return A list containing the inverse or pseudoinverse of the matrix, the determinant for
#'         square invertible matrices, and/or singular value decomposition for non-square or
#'         near-singular matrices. In case of errors or warnings, NULL is returned.
#'
#' @details The function first checks if the input is a matrix, then whether it contains complex
#' values, and if it is square. Based on these checks, appropriate calculations and warnings
#' are issued. A graphical representation of the inverse or pseudoinverse matrix is also plotted.
#'
#' @examples
#' # Square invertible matrix
#' square_invertible_matrix <- matrix(c(1, 2, 3, 4), ncol = 2)
#' result_square <- matrix_invertibility_check(square_invertible_matrix)
#'
#' # Non-square matrix
#' non_square_matrix <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
#' result_non_square <- matrix_invertibility_check(non_square_matrix)
#'
#' @importFrom graphics image
#' @importFrom MASS ginv
matrix_invertibility_check <- function(mat,  epsilon = 0.000001) {
  # Function implementation

    result <- tryCatch(

      # Check if input is a matrix
      if (!is.matrix(mat)) {
        stop("Input must be a matrix.")

      # Check if matrix has complex values
      } else if (any(is.complex(mat))){
        stop("Matrix contains complex values. This feature will be implemented in a future version.")

      # Check if matrix is square
      } else if (ncol(mat) != nrow(mat)) {
        # If not square, return pseudo-inverse and singular value decomposition
        warning("Matrix is not square. Returning pseudo-inverse and singular value decomposition.")
        return(list(
          pseudo_inverse = MASS::ginv(mat),
          singular_value_decomposition = svd(mat)
        )) },

      # Calculate determinant
      det_value <- det(mat),

      # Check if determinant is close to zero
      if (abs(det_value) < epsilon) {
        # If determinant is zero, return singular value decomposition
        warning("Matrix is close to singular (determinant is near zero). Returning singular value decomposition.")
        return(list(singular_value_decomposition = svd(mat)))
      } else {
        # If determinant is non-zero, return inverse and determinant
        return(list(
          inverse = solve(mat),
          determinant = det_value
        ))
      },

    finally = {
      # Plot the inverse or pseudoinverse matrix
      if (!is.null(result) && !is.null(result$inverse)) {
        cat("Graphical representation of the inverse or pseudoinverse matrix:\n")
        image(result$inverse, main = "Inverse or Pseudoinverse Matrix", col = grey.colors(256))
      }
    },
    error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      NULL
    },
    warning = function(w) {
      cat("Warning:", conditionMessage(w), "\n")
      NULL
    }
  )

  return(result)
}

# Example usage:
# Square invertible matrix
# square_invertible_matrix <- matrix(c(1, 2, 3, 4), ncol = 2)
# result_square <- matrix_invertibility_check(square_invertible_matrix)

# Non-square matrix
# non_square_matrix <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
# result_non_square <- matrix_invertibility_check(non_square_matrix)
