#' Function to process the data for a specific transaction type
#' Process Transaction Data
#'
#' This function processes transaction data for a specified type ('buy' or 'sell').
#' It aggregates the total amount for each product per month, creating a matrix
#' where each row represents a product and each column a month.
#'
#' @param df A dataframe containing transaction data with columns 'Date',
#' 'Product', 'Transaction_type', and 'Total_Amount'.
#' @param transaction_type A character string specifying the type of transaction
#' to process, e.g., 'buy' or 'sell'.
#' @return A matrix where each row corresponds to a product and each column to a
#' month. The matrix elements represent the total transaction amount for that
#' product in that month.
#' @examples
#' sample_data <- data.frame(
#'   Date = as.Date(c('2020-01-01', '2020-01-15', '2020-02-01')),
#'   Product = c('ProductA', 'ProductB', 'ProductA'),
#'   Transaction_type = c('Purchase', 'Sale', 'Purchase'),
#'   Total_Amount = c(100, 200, 150)
#' )
#' result <- process_transaction_data(sample_data, 'buy')
#' @export
#'
#' @importFrom dplyr filter mutate group_by summarise ungroup
#' @importFrom tidyr pivot_wider
process_transaction_data <- function(df, transaction_type) {
  # Function implementation

  tryCatch({
    # Check if the necessary columns are present
    required_columns <- c("Date", "Product", "Transaction_Type", "Total_Amount")
    if (!all(required_columns %in% names(df))) {
      stop("Dataframe does not contain all required columns. //
           - Date, Product, Transaction_Type, Total_Amount")
    }

    # Check if transaction_type is valid
    if (!transaction_type %in% unique(df$Transaction_Type)) {
      warning("Specified transaction type not found in the dataframe.")
      return(NULL)
    }

    # Process the data#
#    processed_data <- df %>%
#      filter(Transaction_Type == transaction_type) %>%
#      mutate(
#        YearMonth = format(as.Date(Date), "%Y-%m")
#      ) %>%
#      group_by(Product, YearMonth) %>%
#      summarise(Total = sum(Total_Amount, na.rm = TRUE), .groups = 'drop')

    #      %>%
#      pivot_wider(names_from = YearMonth, values_from = Total) %>%
#      ungroup() %>%
#      select(-Product) %>%
#      as.matrix()

    # Aggregate data by product and month
#    aggregated_data <- df %>%
#      mutate(YearMonth = format(as.Date(Date), "%Y-%m")) %>%
#      group_by(Product, YearMonth) %>%
#      summarise(Total = sum(Total_Amount, na.rm = TRUE), .groups = 'drop')

    aggregated_data <- df %>%
      mutate(
        YearMonth = format(as.Date(Date), "%Y-%m")
      ) %>%
      group_by(Product, YearMonth) %>%
      summarize(Total = sum(Total_Amount, na.rm = TRUE)) %>%
      spread(key = YearMonth, value = Total) %>%
      ungroup() %>%
      select(-Product) %>%
      as.matrix()



    # Reshape data to wide format
 #   wide_data <-  pivot_wider(aggregated_data, names_from = YearMonth,
 #                   values_from = Total, values_fill = list(Total = 0))

     # Setting row names as products
  #  rownames(product_matrix) <- wide_data$Product



    # Remove the first column (Product) and convert to matrix
   # product_matrix <- t(as.matrix(wide_data[-1]))



    return(aggregated_data)
  }, error = function(e) {
    # Handle errors
    warning("Error occurred: ", e$message)
    return(NULL)
  })
}

