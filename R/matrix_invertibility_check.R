matrix_invertibility_check <- function(mat,  epsilon = 0.000001){
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
