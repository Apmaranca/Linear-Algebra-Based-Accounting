# Business Dynamics function
business_dynamics <- function(start_date = '2022-01-01',
                              final_date = '2022-12-31',
                              prices_file = "data/prices.dat") {
  # Generate synthetic street food sales data for a specified number of months
  street_food_sales_data <- generate_street_food_sales_data(start_date,
                                                            final_date,
                                                            prices_file)


  # Get unique products
  unique_products <- unique(street_food_sales_data$Product)

  # Initialize matrices to store monthly totals for buying and selling
  buying_matrix <- matrix(0, nrow = length(unique_products), ncol = 0)
  selling_matrix <- matrix(0, nrow = length(unique_products), ncol = 0)

  current_date <- as.Date(start_date)
  num_months <- floor(as.numeric(interval(start_date, final_date) / months(1)))

  # Loop through each month
  M_coefficients_list <- list()

  # Assuming street_food_sales_data is your data frame
  unique_dates <- unique(street_food_sales_data$Date)

  # Truncate dates to the beginning of the month
  truncated_dates <- ym(unique_dates)

  # Get unique months
  unique_months <- unique(truncated_dates)

  # Calculate the number of unique months
  num_months <- length(unique_months)

  # Calculate the number of unique dates
  num_unique_dates <- length(unique_dates)

  # Loop through each month
  for (monthloop in 1:num_months) {

        # Get the date of fist day of current month
        current_date <- floor_date(ymd(unique_months[monthloop]),
                                   unit = "month")

        # Subset data for the current month
monthly_data <- summarise(
              street_food_sales_data,
              ym(street_food_sales_data$Date) == unique_months[monthloop]
                      )

    # Update buying_matrix and selling_matrix based on monthly totals
    buying_vector <- rep(0, length(unique_products))
    selling_vector <- rep(0, length(unique_products))

    for (i in seq_along(unique_products)) {
      product <- unique_products[i]
      buying_vector[i] <- sum(
        monthly_data$Total_Amount[monthly_data$Transaction_Type ==
                                'Purchase' & monthly_data$Product == product])
      selling_vector[i] <- sum(
        monthly_data$Total_Amount[monthly_data$Transaction_Type ==
                                    'Sale' & monthly_data$Product == product])
    }

    # Append buying_vector and selling_vector to the matrices
    buying_matrix <- cbind(buying_matrix, buying_vector)
    selling_matrix <- cbind(selling_matrix, selling_vector)

    # Find the best fit using multiple linear regression for a linear matrix M
    #for the current month
    M_coefficients <- find_linear_matrix_M(selling_vector, buying_matrix)
    M_coefficients_list[[monthloop]] <- M_coefficients

    # Move to the next month
  }

  # Create the "output" directory if it doesn't exist
  dir.create("output", showWarnings = FALSE)

  # Save series of s, b, and M in CSV files in the "output" directory for each month
  for (monthloop in seq_along(M_coefficients_list)) {
    suffix <- ifelse(file.exists(paste0("output/s_series_",
                                        format(current_date, "%Y%m"),
                                        ".csv")), "_v2", "")
    write.csv(data.frame(s = selling_matrix[, monthloop],
                         Product = unique_products),
              file = paste0("output/s_series_",
                            format(current_date, "%Y%m"),
                            suffix, ".csv"), row.names = FALSE)
    write.csv(data.frame(b = buying_matrix[, monthloop], Product = unique_products),
              file = paste0("output/b_series_", format(current_date, "%Y%m"),
                            suffix, ".csv"), row.names = FALSE)
    write.csv(data.frame(M = M_coefficients_list[[monthloop]],
                         Product = unique_products),
              file = paste0("output/M_matrix_",
                            format(current_date, "%Y%m"),
                            suffix, ".csv"), row.names = FALSE)
  }

  # Plot singular values for each matrix M over time
  plot_singular_values(M_coefficients_list)
}
