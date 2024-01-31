
# Function to find the best fit using multiple linear regression for a linear matrix M
find_linear_matrix_M <- function(selling_vector, buying_matrix) {
  # Combine selling_vector and buying_matrix into a data frame
  data_df <- data.frame(selling_vector, buying_matrix)

  # Perform multiple linear regression
  model <- lm(selling_vector ~ ., data = data_df)

  # Extract the coefficients
  M_coefficients <- matrix(coef(model)[-1], ncol = ncol(buying_matrix))

  return(M_coefficients)
}
