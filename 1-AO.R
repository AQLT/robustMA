library(rjd3filters)
source("0-functions.R")
lc_f <- lp_filter()
robust_filters <- readRDS("data/robust_filters.rds")


data <- readRDS("data/data.rds")

# IPI voitures: 010768140
ipi_voitures <- data[["010768140"]]
date_out <- c(1998 + (8-1)/12, 1998 + (8-1)/12)
y <- window(ipi_voitures, start = min(date_out) - 2, end = max(date_out)+2)
dates_studied <- time(y)[-(1:18)]
y_vintage <- lapply(dates_studied, window, x = y, start = NULL)
names(y_vintage) <- dates_studied
lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ao"]], date_out)

date_out <- c(2004 + (8-1)/12)
y <- window(ipi_voitures, start = min(date_out) - 2, end = max(date_out)+2)
dates_studied <- time(y)[-(1:18)]
y_vintage <- lapply(time(y)[-(1:18)], window, x = y, start = NULL)
lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ao"]], date_out)


# IPI voitures: 010768140
CE16OV <- data[["CE16OV"]]
date_out <- 2001 + (2 - 1) / 12
y <- window(CE16OV, start = min(date_out) - 2, end = max(date_out)+2)
dates_studied <- time(y)[-(1:18)]
y_vintage <- lapply(dates_studied, window, x = y, start = NULL)
names(y_vintage) <- dates_studied
res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ao"]], date_out)
plot(window(res[[20]][,1], start = 2000))
lines(res[[20]][,2], col = "orange")
lines(res[[20]][,3], col = "lightblue")
lines(res[[20]][,4], col = "darkgreen")

RETAILx <- data[["RETAILx"]]
date_out <- 2007 + (11 - 1) / 12
y <- window(RETAILx, start = min(date_out) - 2, end = max(date_out)+2)
dates_studied <- time(y)[-(1:18)]
y_vintage <- lapply(dates_studied, window, x = y, start = NULL)
names(y_vintage) <- dates_studied
res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ao"]], date_out)
plot(window(res[[20]][,1], start = 2006, end = 2008.5))
lines(res[[20]][,2], col = "orange")
lines(res[[20]][,3], col = "lightblue")
lines(res[[20]][,4], col = "darkgreen")
