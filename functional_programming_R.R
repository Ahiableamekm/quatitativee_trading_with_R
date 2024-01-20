# The goal of this project is to leverage R's power of functional programming 
# to compute pairwise correlations of stock tickers

# To achieve this we can decompose the project into the following task

# - 1. Obtain the names of the stocks
# - 2. Make sure the are valid stocks
# - 3. Connect to a database that has this stock and pull the infomation into memmory
# - 4. Clean up the data by identifying missing values
# - 5. Put all the data into a data frame
# - 6. Compute the pairwise correlation and return the results
# - 7. create a visual of pairwise correlation.


# - 1.

filter_and_sort_symbols <- function(symbols) {
  # Name: filter_symbols
  # Purpose: convert to uppercase if not in
  # remove all non valid symbols
  # Input: symbols = vector of stock tickers
  # Output: filtered_symbols = filtered symbols
  
  # convert symbols to uppercase
  symbols <- toupper(symbols)
  
  # validate the symbol names
  valid <- regexpr("^[A-Z]{2,4}$", symbols)
  
  # return only the valid symbols
  return(sort(symbols[valid==1]))
}



# - 2.
extract_prices <- function(filtered_symbols, file_path){
  # Name: extract_prices
  # Purpose: Read price data from specified file
  # Input: filtered_symbol = vector of symbols
          # file_path = location of price data 
  # Output: prices = data.frame of price per symbol
  
  # Read in the csv file
  all_prices = read.csv(file = file_path, header = TRUE, stringsAsFactors =  FALSE)
  
  # Make the Date row name
  rownames(all_prices) <- all_prices$Date
  
  # Remove original Date column
  all_prices$Date <- NULL
  
  # Extract only the relevant data columns
  valid_columns <- colnames(all_prices) %in% filtered_symbols
  
  return(all_prices[, valid_columns])
  
}


# - 3.

filtered_prices <- function(prices){
  # Name: filter_prices
  # Purpose: Identify the rows with missing values
  # Input: prices = data.frame of prices
  # Output: missing_rows = vector of indexes where
  # data is missing in any of the columns
  
  # return a boolean vector of good or bad rowa
  valid_rows = complete.cases(prices)
  
  # Identify the index of missing rows
  missing_rows <- which(valid_rows == FALSE)
  
  return(missing_rows)
}


# - 4.
compute_pairwise_correlation <- function(prices) {
  # Name: compute_pairwise_correlation
  # Purpose: Calculate the pairwise correlation of returns
  # and plots the pairwise relationships
  # Input: prices = data.frame of prices
  # Output: correlation_matrix = A correlation matrix
  
  # convert prices to returns
  returns <- apply(prices, 2, function(x) diff(log(x)))
  
  # Plot all pairwise relationship
  pairs(returns, main = "Pairwise return scatter plot")
  
  correlation_matrix <- cor(returns, use = "complete.obs")
  
  return(correlation_matrix)
}

# symbols entered by the user
symbols = c("IBM", "XOM", "2SG", "TEva", "GOog", "CVX", "AAPL", "BA")
print(symbols)

# database path
file_path = "C:/Users/user/project_directory/quatitativee_trading_with_R/prices.csv"

# filter and sort the symbols
filtered_symbols <- filter_and_sort_symbols(symbols)
print(filtered_symbols)


# Extract the prices
prices <- extract_prices(filtered_symbols, file_path)
print(prices)

# filter the prices
missing_row <- filtered_prices(prices)
print(missing_row)


# compute correlations
correlation_matrix <- compute_pairwise_correlation(prices)
