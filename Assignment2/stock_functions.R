# ==============================================================================
# BDA400 - Assignment 2: Technical Analysis using R (Preliminary Stage)
# File   : stock_functions.R
# Purpose: Functions to (1) import stock data listed in portfolio.txt using
#          quantmod and (2) calculate basic statistics for each stock.
# Usage  : source("stock_functions.R")
# ==============================================================================

# install.packages(c("quantmod", "TTR"))   # run once if not yet installed
suppressPackageStartupMessages({
  library(quantmod)   # getSymbols() - downloads stock data
  library(TTR)        # SMA()        - moving average
})

# ---- Configuration -----------------------------------------------------------
PORTFOLIO_FILE <- "portfolio.txt"
START_DATE     <- as.Date("2023-01-01")
END_DATE       <- Sys.Date()
MA_WINDOW      <- 20          # moving-average window in trading days

# ==============================================================================
# 1. IMPORT FUNCTIONS
# ==============================================================================

# Read stock symbols (one per line) from the portfolio file.
read_portfolio <- function(file = PORTFOLIO_FILE) {
  if (!file.exists(file)) {
    stop("Portfolio file not found: ", file,
         "\nCurrent working directory: ", getwd())
  }
  symbols <- trimws(readLines(file, warn = FALSE))
  symbols <- toupper(symbols[nzchar(symbols)])   # drop blank lines, upper-case
  unique(symbols)
}

# Read portfolio.txt and import the data for each symbol with quantmod.
# Returns a named list; each element is a separate data frame:
#   Date, Open, High, Low, Close, Volume, Adjusted
load_stock_data <- function(file = PORTFOLIO_FILE,
                            from = START_DATE, to = END_DATE) {
  symbols <- read_portfolio(file)
  message("Symbols found in ", file, ": ", paste(symbols, collapse = ", "))

  stocks <- list()
  for (sym in symbols) {
    result <- tryCatch({
      xts_data <- getSymbols(sym, src = "yahoo", from = from, to = to,
                             auto.assign = FALSE)
      df <- data.frame(Date = index(xts_data), coredata(xts_data),
                       row.names = NULL)
      names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
      df <- df[!is.na(df$Close), ]
      message("  loaded ", sym, ": ", nrow(df), " rows")
      df
    }, error = function(e) {
      warning("Could not load ", sym, ": ", conditionMessage(e), call. = FALSE)
      NULL
    })
    if (!is.null(result)) stocks[[sym]] <- result
  }
  if (length(stocks) == 0) stop("No stock data could be loaded.")
  stocks
}

# ==============================================================================
# 2. STATISTICS FUNCTIONS
# ==============================================================================

# Base R has no statistical mode, so define one (most frequent value).
calc_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Calculate statistics for ONE stock's data frame (closing price).
# Returns a list:
#   $summary        data frame: Statistic / Value
#   $moving_average data frame: Date, Close, MA (full series)
calculate_statistics <- function(df, window = MA_WINDOW) {
  close <- df$Close
  ma    <- SMA(close, n = window)

  summary_df <- data.frame(
    Statistic = c(paste0("Moving Average (", window, "-day, latest)"),
                  "Mean", "Mode", "Median", "Standard Deviation"),
    Value = c(tail(ma[!is.na(ma)], 1),
              mean(close),
              calc_mode(round(close, 2)),   # prices rounded to cents
              median(close),
              sd(close))
  )
  summary_df$Value <- round(summary_df$Value, 4)

  list(summary = summary_df,
       moving_average = data.frame(Date = df$Date, Close = close, MA = ma))
}

# Apply calculate_statistics() to every stock in the list.
calculate_all_statistics <- function(stocks, window = MA_WINDOW) {
  lapply(stocks, calculate_statistics, window = window)
}
