
# Function to find the best fit using multiple linear regression for a
# linear matrix M
find_linear_matrix_M <- function(selling_vector, buying_vector) {

    unit_buying_vector <- buying_vector / norm_vec(buying_vector)^2

    M_coefficients <- outer(selling_vector, unit_buying_vector, FUN = "*")

  return(M_coefficients)
}

norm_vec <- function(x){
  sqrt(sum(x^2))
}
