# ==============================================================================
# BDA400 - Assignment 2: Technical Analysis using R (Preliminary Stage)
# File   : display_stocks.R
# Purpose: Utilities to DISPLAY the imported stock data, the calculated
#          statistics, and visualizations. Requires stock_functions.R.
# Usage  : Set the working directory to this folder, then run
#          source("display_stocks.R")   (or Rscript display_stocks.R)
# ==============================================================================

# install.packages("ggplot2")   # run once if not yet installed
suppressPackageStartupMessages(library(ggplot2))
source("stock_functions.R")

# ---- Text displays -----------------------------------------------------------

# (1) Data frame view: dimensions, date range, first and last rows.
display_stock_data <- function(stocks, n = 5) {
  for (sym in names(stocks)) {
    d <- stocks[[sym]]
    cat("\n==================== ", sym, " ====================\n", sep = "")
    cat("Rows:", nrow(d), "| From:", format(min(d$Date)),
        "to", format(max(d$Date)), "\n")
    cat("\nFirst", n, "rows:\n"); print(head(d, n), row.names = FALSE)
    cat("\nLast",  n, "rows:\n"); print(tail(d, n), row.names = FALSE)
  }
  invisible(NULL)
}

# (2) Yahoo-Finance-style quote summary: last close, day change, day range,
#     52-week range, volume.
display_quote_summary <- function(stocks) {
  rows <- lapply(names(stocks), function(sym) {
    d    <- stocks[[sym]]
    last <- d[nrow(d), ]
    prev <- if (nrow(d) > 1) d$Close[nrow(d) - 1] else NA
    year <- d[d$Date > (max(d$Date) - 365), ]
    data.frame(Symbol = sym, Date = format(last$Date),
               Close = round(last$Close, 2),
               Change = round(last$Close - prev, 2),
               `Change %` = round(100 * (last$Close - prev) / prev, 2),
               `Day Range` = sprintf("%.2f - %.2f", last$Low, last$High),
               `52wk Range` = sprintf("%.2f - %.2f", min(year$Low), max(year$High)),
               Volume = format(last$Volume, big.mark = ","),
               check.names = FALSE)
  })
  do.call(rbind, rows)
}

# (3) Statistics table for each stock.
display_statistics <- function(stats) {
  for (sym in names(stats)) {
    cat("\n--- Statistics for", sym, "(closing price) ---\n")
    print(stats[[sym]]$summary, row.names = FALSE)
  }
  invisible(NULL)
}

# (4) Side-by-side statistics for all stocks.
statistics_comparison <- function(stats) {
  tbl <- do.call(cbind, lapply(stats, function(s) s$summary$Value))
  rownames(tbl) <- stats[[1]]$summary$Statistic
  as.data.frame(tbl)
}

# ---- Visualizations ----------------------------------------------------------

# (5) Closing price with moving average.
plot_price_ma <- function(stats, sym, window = MA_WINDOW) {
  d <- stats[[sym]]$moving_average
  ma_label <- paste0(window, "-day MA")
  cols <- setNames(c("grey45", "#1f77b4"), c("Close", ma_label))
  ggplot(d, aes(Date)) +
    geom_line(aes(y = Close, colour = "Close"), linewidth = 0.5) +
    geom_line(aes(y = MA, colour = ma_label), linewidth = 0.9, na.rm = TRUE) +
    scale_colour_manual(values = cols) +
    labs(title = paste(sym, "- Closing Price and Moving Average"),
         x = NULL, y = "Price (USD)", colour = NULL) +
    theme_minimal() + theme(legend.position = "bottom")
}

# (6) Candlestick chart with volume (quantmod) for the last N trading days.
plot_candles <- function(stocks, sym, days = 90) {
  d <- tail(stocks[[sym]], days)
  x <- xts::xts(d[, c("Open", "High", "Low", "Close", "Volume")],
                order.by = d$Date)
  chartSeries(x, name = paste(sym, "- last", days, "trading days"),
              type = "candlesticks", theme = chartTheme("white"), TA = "addVo()")
}

# (7) Histogram of closing prices with mean (red) and median (green).
plot_close_hist <- function(stocks, stats, sym) {
  s <- stats[[sym]]$summary
  ggplot(stocks[[sym]], aes(Close)) +
    geom_histogram(bins = 30, fill = "#1f77b4", colour = "white") +
    geom_vline(xintercept = s$Value[s$Statistic == "Mean"],
               colour = "red", linetype = "dashed") +
    geom_vline(xintercept = s$Value[s$Statistic == "Median"],
               colour = "darkgreen", linetype = "dotted") +
    labs(title = paste(sym, "- Distribution of Closing Prices"),
         subtitle = "Red dashed = mean, green dotted = median",
         x = "Close (USD)", y = "Number of days") +
    theme_minimal()
}

# (8) All stocks on one chart, indexed to 100 at the start date.
plot_normalised <- function(stocks) {
  d <- do.call(rbind, lapply(names(stocks), function(sym) {
    x <- stocks[[sym]]
    data.frame(Date = x$Date, Symbol = sym, Index = 100 * x$Close / x$Close[1])
  }))
  ggplot(d, aes(Date, Index, colour = Symbol)) +
    geom_line() +
    labs(title = "Portfolio Comparison (closing price indexed to 100)",
         x = NULL, y = "Index") +
    theme_minimal()
}

# (9) Bar chart comparing standard deviation of each stock.
plot_sd_comparison <- function(stats) {
  d <- data.frame(
    Symbol = names(stats),
    SD = sapply(stats, function(s) s$summary$Value[s$summary$Statistic == "Standard Deviation"]))
  ggplot(d, aes(reorder(Symbol, SD), SD)) +
    geom_col(fill = "#1f77b4") +
    labs(title = "Standard Deviation of Closing Price", x = NULL, y = "USD") +
    theme_minimal()
}

# ---- Run everything ----------------------------------------------------------
run_analysis <- function() {
  stocks <- load_stock_data()
  stats  <- calculate_all_statistics(stocks)

  display_stock_data(stocks)
  cat("\n=== Quote summary ===\n");       print(display_quote_summary(stocks), row.names = FALSE)
  display_statistics(stats)
  cat("\n=== Statistics comparison ===\n"); print(statistics_comparison(stats))

  for (sym in names(stocks)) {
    print(plot_price_ma(stats, sym))
    print(plot_close_hist(stocks, stats, sym))
  }
  print(plot_normalised(stocks))
  print(plot_sd_comparison(stats))
  plot_candles(stocks, names(stocks)[1])
  invisible(list(stocks = stocks, stats = stats))
}

# Runs automatically unless an R Markdown file sets options(ta.no_main = TRUE)
if (!isTRUE(getOption("ta.no_main"))) results <- run_analysis()

