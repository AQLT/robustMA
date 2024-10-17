library(rjd3filters)
source("0-functions.R")
lc_f <- lp_filter()

robust_filters <- readRDS("data/robust_filters.rds")

data <- readRDS("data/data.rds")

date_out <- 2010 + (6 - 1) / 12
y <- window(data[["010767578"]], start = min(date_out) - 2, end = max(date_out)+2)
dates_studied <- time(y)[-(1:18)]
y_vintage <- lapply(dates_studied, window, x = y, start = NULL)
names(y_vintage) <- dates_studied
res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ls"]], date_out)
AQLTools::graph_ts(res$"2011"[,1:4])

