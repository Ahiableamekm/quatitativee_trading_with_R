# loading, handling and plotting financial data with packages like:
# - 1. `xts` for creating time series object and handling time series data
# - 2. `quantmod` for pulling online and offline financial data and plotting
# - 3. `ggplot2` Grammer of graphics for ploting


# Installing packages install.packages(c("xts", "quantmod", "ggplot2"))
# load in needed library either with `library` or `require`
library(xts)
library(quantmod)
library(ggplot2)

# Eploring xts with sample_matrix data that comes with the package

# load data
data(sample_matrix)

# checking the head of the data
head(sample_matrix)

# checking the object type of sameple_matrix
class(sample_matrix)

# Getting detail info on the matrix object
str(sample_matrix)


# converting sample_matrix data from a matrix.array object to xts.zoo object
xts_data <- as.xts(sample_matrix, descr = "sample_matrix convert to xts.zoo object")

# Rechecking the structure on the new xts object makes the info somewhat more readable
str(xts_data)

# plotting one column of the data
plot(xts_data[,1], main = "plot of time series data", cex.main = 0.8)

# subsetting only some rows to plot
plot(xts_data["2007-01-01::2020-02:12"], main = "Subset of the data", cex.main = 0.8)

# other ways to provide the subset
date_range <- "2007-03-15::2007-06-15"
plot(xts_data[date_range])

# using paste to combine date ranges for the plot
start_date <- "2007-05-05"
end_date <- "2007-12-31"
plot(xts_data[paste(start_date,end_date, sep = "::")])

# creating sample time series data to explore xts time series capapilities
price_vector <- c(101.02, 101.03, 101.03, 101.04, 101.05,
                  101.03, 101.02, 101.01, 101.00, 100.99)

dates <- c("03/12/2013 08:00:00.532123",
           "03/12/2013 08:00:01.982333",
           "03/12/2013 08:00:01.650321",
           "03/12/2013 08:00:02.402321",
           "03/12/2013 08:00:02.540432",
           "03/12/2013 08:00:03.004554",
           "03/12/2013 08:00:03.900213",
           "03/12/2013 08:00:04.050323",
           "03/12/2013 08:00:04.430345",
           "03/12/2013 08:00:05.700123")
options(digits.sec = 6)

# creating time index in a correct format
time_index <- strptime(dates, format = "%d/%m/%Y %H:%M:%OS")


# create xts object with price_vector and time_index
xts_price_vector <- xts(price_vector, time_index)


# plot the Xts_price_vector
plot(xts_price_vector, main = "Sample time serie data")


# adding a horizontal line on the mean
abline(h = mean(xts_price_vector), lwd = 2)
grid(lty = 0)


# adding a vertical line at a specified time stamp
my_time <- as.POSIXct("03/12/2013 08:00:03.004554", format = "%d/%m/%Y %H:%M:%OS")
abline(v = my_time, lwd = 2, lty = 2)




# - 2.

# Eploring quantmod

# start by downloading a AAPL stock online for exploration

AAPL <- getSymbols("AAPL", auto.assign = FALSE)

## checking the class of AAPL
class(AAPL)

# charting with quantmod and adding some technical indicators
chartSeries(AAPL,theme = chartTheme("white"), TA = "addVo(); addBBands()")



# updating the chart with new data without re-specifying all chart parameters
reChart(subset = "2020-01-10::2024-01-19")


# adding other technical indicators
chartSeries(AAPL, theme = chartTheme("white"), TA = "addBBands(); addDEMA()")


# creating one own indicator

# start by charting with no indicator
chartSeries(AAPL, theme = chartTheme("white"), TA = NULL)

# creating indicator function
my_indicator <- function(x) {
  return(x + 90)
}

add_my_indicator <- newTA(FUN = my_indicator, preFUN = Cl, on = 1, legend.name = "My fancy indicator")


# re-plotting with the customized indicator
add_my_indicator()


# - 3.

# Exploring ggplot2

# start by creating a dataframe form AAPL

df <- AAPL[, c("AAPL.Adjusted", "AAPL.Volume")]

# rename the columns
names(df) <- c("price", "volume")

head(df)



# creating a new column the hold the log returns of price
df$returns <- diff(log(df[,1]))
head(df)


# removing the first NA values in the returns column
df <- df[-1, ]
head(df)



# segmenting the returns in to bins
df$cuts <- cut(abs(df$returns), breaks = c(0, 0.02, 0.04, 0.25), include.lowest = TRUE)


# creating mean columns
df$mean <- NA

for (i in 1:3) {
  group <- which(df$cuts == i)
  if (length(group) > 0){
    df$mean[group] <- mean(df$volume[group])
  }
}
head(df)


# plotting result
ggplot(df) + geom_histogram(aes(x=volume)) + facet_grid(cuts ~ .) + geom_vline(aes(xintercept=mean), linetype = "dashed", size=1)
