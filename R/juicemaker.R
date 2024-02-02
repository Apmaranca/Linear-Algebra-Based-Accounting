#' Generate Synthetic Street Food Sales Data
#'
#' This function generates synthetic sales data for street food transactions
#' over a specified period. It optionally reads prices from a file and generates
#' sales data based on these prices. If the prices file is not found or has an
#' invalid format, default pricing data is used.
#'
#' @param start_date The start date for generating sales data in 'YYYY-MM-DD' format.
#'                   If not provided, defaults to '2022-01-01'.
#' @param final_date The end date for generating sales data in 'YYYY-MM-DD' format.
#'                   If not provided, defaults to '2022-12-31'.
#' @param prices_file The path to the CSV file containing product prices.
#'                    Defaults to "data/prices.dat".
#'
#' @return A dataframe containing the synthetic sales data with columns for the date,
#'         product, transaction type, and total amount.
#'
#' @details The function checks for the existence of the specified prices file. If the
#'          file is not found or is incorrectly formatted, default prices are used.
#'          Sales data is then generated for each day between the start and end dates
#'          for each product, with randomly selected transaction types and quantities.
#'
#' @examples
#' # Generate sales data with default parameters
#' sales_data <- generate_street_food_sales_data()
#'
#' # Generate sales data for a specific period
#' sales_data <- generate_street_food_sales_data('2022-01-01', '2022-12-31')
#'
#' @importFrom utils read.csv write.csv
#' @importFrom lubridate ymd
#' @export
generate_street_food_sales_data <- function(start_date = '2022-01-01',
                                            final_date = '2022-12-31',
                                            prices_file = "data/prices.dat") {
  # Function implementation

  tryCatch({
    if (missing(start_date)) {
      start_date <- ymd('2022-01-01')
    } else {
      start_date <- ymd(start_date)
    }
    if (missing(final_date)) {
      final_date <- ymd('2022-12-31')
    } else {
      final_date <- ymd(final_date)
    }

    if (file.exists(prices_file)) {
      prices_data <- read.csv(prices_file, header = TRUE)

      if (ncol(prices_data) != 2 || nrow(prices_data) < 1) {
        warning("Invalid format or length of prices data. Using default data.")
      }
    } else {
      unit_prices <- c(2.5, 3, 2.8, 1, 1.2, 0.5, 0.1, 0.3)
      prices_data <- data.frame(
        Product = paste0("Product", 1:length(unit_prices)),
        Unit_Price = unit_prices,
        stringsAsFactors = FALSE
      )
      write.csv(prices_data, file = prices_file, row.names = FALSE)
    }


    provint <- as.numeric(difftime(start_date, final_date, units = "days"))

    date_sequence <- seq(start_date, by = '1 day',
                         length.out = abs(provint) + 1)

    sales_data <- data.frame(Date = character(),
                             Product = character(),
                             Transaction_Type = character(),
                             Total_Amount = numeric(),
                             stringsAsFactors = FALSE)

    for (current_date in date_sequence) {
      for (product in prices_data$Product) {
        transaction_type <- sample(c("Purchase", "Sale"), 1, prob = c(0.4, 0.6))
        quantity <- sample(1:10, 1)
        unit_price <- prices_data$Unit_Price[prices_data$Product == product]
        total_amount <- quantity * unit_price

        sales_data <- rbind(sales_data,
                            data.frame(Date = current_date,
                                       Product = product,
                                       Transaction_Type = transaction_type,
                                       Total_Amount = total_amount,
                                       stringsAsFactors = FALSE))
      }
    }

    return(sales_data)
  }, warning = function(w) {
    print("Warning processing prices data. Using default data.")

    unit_prices <- c(2.5, 3, 2.8, 1, 1.2, 0.5, 0.1, 0.3)
    prices_data <- data.frame(
      Product = paste0("Product", 1:length(unit_prices)),
      Unit_Price = unit_prices,
      stringsAsFactors = FALSE
    )
    write.csv(prices_data, file = prices_file, row.names = FALSE)

    start_date <- ymd(start_date)
    final_date <- ymd(final_date)
    length.out = as.numeric(difftime(start_date, final_date, units = "days"))
    length.out = abs(length.out)+1
    date_sequence <- seq(
                         start_date, by = '1 day',
                         length.out,
                         )
    sales_data <- data.frame(
                         Date = character(),
                         Product = character(),
                         Transaction_Type = character(),
                         Total_Amount = numeric(),
                         stringsAsFactors = FALSE
                          )


    for (current_date in date_sequence) {
      for (product in prices_data$Product) {
        transaction_type <- sample(c("Purchase", "Sale"), 1, prob = c(0.4, 0.6))
        quantity <- sample(1:10, 1)
        unit_price <- prices_data$Unit_Price[prices_data$Product == product]
        total_amount <- quantity * unit_price

        sales_data <- rbind(sales_data,
                            data.frame(Date = current_date,
                                       Product = product,
                                       Transaction_Type = transaction_type,
                                       Total_Amount = total_amount,
                                       stringsAsFactors = FALSE))
      }
    }

    return(sales_data)
  })
}
