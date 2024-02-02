#' Function implementation
#' Business Dynamics Analysis
#'
#' Analyzes the dynamics of street food sales based on synthetic data, generating
#' insights on buying and selling patterns over a specified time period. The function
#' processes transaction data to compute buying and selling matrices, modifies these
#' matrices based on juice restrictions, calculates a series of linear matrices representing
#' monthly business dynamics, and plots the singular values of these matrices.
#'
#' @param start_date Start date for the analysis period in 'YYYY-MM-DD' format.
#'        Default is '2022-01-01'.
#' @param final_date Final date for the analysis period in 'YYYY-MM-DD' format.
#'        Default is '2022-12-31'.
#' @param prices_file File path for the prices data, defaulting to "data/prices.dat".
#'
#' @details The function starts by generating synthetic street food sales data between
#' the specified start and final dates. It then processes this data to extract buying
#' and selling patterns. These patterns are adjusted based on juice restrictions.
#' The function computes a series of linear matrices M for each month, representing
#' the relationship between buying and selling patterns. Finally, it visualizes
#' the singular values of these matrices over time.
#'
#' The function creates and saves various CSV files in an 'output' directory,
#' representing the monthly buying and selling series, as well as the computed matrices M.
#'
#' @return No return value, this function is used for its side effects: generating plots
#' and writing CSV files.
#'
#' @examples
#' # Assuming generate_street_food_sales_data, process_transaction_data,
#' # find_linear_matrix_M, and plot_singular_values are defined:
#' business_dynamics()
#'
#' @importFrom ggplot2 ggplot geom_line labs theme_minimal
#' @importFrom dplyr filter mutate group_by summarise ungroup
#' @importFrom utils write.csv

# deprecated @importFrom svd

business_dynamics <- function(start_date = '2022-01-01',
                              final_date = '2022-12-31',
                              prices_file = "extdata/prices.dat") {


  # Generate synthetic street food sales data for a specified number of months
  street_food_sales_data <- generate_street_food_sales_data(start_date,
                                                            final_date,
                                                            prices_file)

  # Extract unique products representing monthly buyings from the data
  buying_matrix_theor <- process_transaction_data(street_food_sales_data,
                                                  "Purchase")

  # Extract unique products representing monthly sellings from the data
  selling_matrix_theor <- process_transaction_data(street_food_sales_data,
                                                   "Sale")



  # Get unique products
  unique_products <- unique(street_food_sales_data$Product)

      # Load restrictions of the dynamics
    juice_restrictions <- read_or_create_juice_data()
#    juice_restrictions$Rb  # Accessing the Rb vector
#    juice_restrictions$Rs  # Accessing the Rs vector


    # Append buying_vector and selling_vector to the matrices
    buying_matrix <- sweep(buying_matrix_theor, 1, juice_restrictions$Rb, "*")
    selling_matrix <- sweep(selling_matrix_theor, 1, juice_restrictions$Rs,"*")

    # Find the best fit using multiple linear regression for a linear matrix M
    #for the current month
    for (monthlook in 1:ncol(buying_matrix)) {
       if(monthlook == 1){
        M_coefficients_list <- array(NA, dim = c(length(juice_restrictions$Rb),
                                                 length(juice_restrictions$Rs),
                                                 ncol(buying_matrix)))
                         }

      M_coefficients_list <- find_linear_matrix_M(
                                            selling_matrix[, monthlook],
                                             buying_matrix[, monthlook])

     }
#   M_coefficients <- find_linear_matrix_M(selling_matrix, buying_matrix)
#  M_coefficients_list[[monthloop]] <- M_coefficients


  # Create the "output" directory if it doesn't exist
  dir.create("output", showWarnings = FALSE)

  # Save series of s, b, and M in CSV files in the "output" directory for
  # each month
  for (monthloop in 1:ncol(buying_matrix)) {

     suffix <- paste0("_", monthloop, "_")

     nomprov <- paste0("output/s_series_",
                       lubridate::today(),
                       ".csv")
    if (file.exists(nomprov)){
      nomprov <- paste0(nomprov, suffix)
                             }

    write.csv(data.frame(s = selling_matrix[, monthloop],
                         Product = unique_products),
              file = find_unique_filename(paste0("output/s_series_",
                            lubridate::today(),
                            suffix, ".csv"), directory = getwd()),
              row.names = FALSE)
    write.csv(data.frame(b = buying_matrix[, monthloop],
                         Product = unique_products),
              file = find_unique_filename(paste0("output/b_series_",
                                                 lubridate::today(),
                            suffix, ".csv"), directory = getwd()),
              row.names = FALSE)
    write.csv(data.frame(M = M_coefficients_list[monthloop],
                         Product = unique_products),
              file = find_unique_filename(paste0("output/M_matrix_",
                            lubridate::today(),
                            suffix, ".csv"), directory = getwd()),
              row.names = FALSE)
  }

  # Plot singular values for each matrix M over time
  plot_singular_values(M_coefficients_list)
}
