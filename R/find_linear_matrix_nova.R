find_linear_matrix_nova_M <- function(data, dependent_vars, independent_vars) {
  # Construct the formula for manova
  # dependent_vars should be a vector of dependent variable names
  # independent_vars should be a string with independent variables in formula syntax
  manova_formula <- as.formula(paste(paste(dependent_vars, collapse = " + "), "~", independent_vars))

  # Fit the multivariate model
  manova_fit <- manova(manova_formula, data = data)

  # Extract coefficients for each dependent variable
  coefs <- lapply(dependent_vars, function(var) {
    # Extracting coefficients for each response
    unname(coef(summary(manova_fit, test = "Pillai")[[var]]))
  })

  # Combine the coefficients into a matrix
  coef_matrix <- do.call(cbind, coefs)

  return(coef_matrix)
}

# Example usage
# Assuming `data` is your dataframe, `dep_vars` is a vector of dependent variable names,
# and `ind_vars` is a string of independent variable names in formula syntax
# coef_matrix <- find_linear_matrix_M(data, dep_vars, ind_vars)
