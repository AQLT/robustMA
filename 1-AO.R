library(rjd3filters)
source("0-functions.R")
lc_f <- lp_filter()

filters_ao <- lapply(6:-6, function(t) {
	X <- c(rep(0, 6+t), 1, rep(0, 6-t))

	sym <- sym_filter(X= X)
	res <- list("q=6" = sym)
	if (t == 6)
		return(res)
	all_q <- 5:max(t, 0)
	afilters <- lapply(all_q, function(q) {
		mmsre_filter(
			ref_filter = sym, q = q,
			delta = 2 / (sqrt(pi) * 3.5),
			U = cbind(polynomial_matrix(l = -6, d0 = 0, d1 = 0), X),
			Z = polynomial_matrix(l = -6, d0 = 1, d1 = 1),
		)
	})
	names(afilters) <- paste0("q=", all_q)
	res <- c(res, afilters)
	res
})
names(filters_ao) <- paste0("t=", 6:-6)

filters_ao <- reorder_out_filters(filters_ao)


data <- readRDS("data/data.rds")

# IPI voitures: 010768140
ipi_voitures <- data[["010768140"]]
date_ao <- c(1998 + (8-1)/12, 1998 + (8-1)/12)
y <- window(ipi_voitures, start = min(date_ao) - 2, end = max(date_ao)+2)
dates_studied <- time(y)[-(1:18)]
y_vintage <- lapply(dates_studied, window, x = y, start = NULL)
names(y_vintage) <- dates_studied
lapply(y_vintage, all_filtered, 13, lc_f, filters_ao, date_ao)

date_ao <- c(2004 + (8-1)/12)
y <- window(ipi_voitures, start = min(date_ao) - 2, end = max(date_ao)+2)
dates_studied <- time(y)[-(1:18)]
y_vintage <- lapply(time(y)[-(1:18)], window, x = y, start = NULL)
lapply(y_vintage, all_filtered, 13, lc_f, filters_ao, date_ao)


# IPI voitures: 010768140
CE16OV <- data[["CE16OV"]]
date_ao <- 2001 + (2 - 1) / 12
y <- window(CE16OV, start = min(date_ao) - 2, end = max(date_ao)+2)
dates_studied <- time(y)[-(1:18)]
y_vintage <- lapply(dates_studied, window, x = y, start = NULL)
names(y_vintage) <- dates_studied
res <- lapply(y_vintage, all_filtered, 13, lc_f, filters_ao, date_ao)
plot(window(res[[20]][,1], start = 2000))
lines(res[[20]][,2], col = "orange")
lines(res[[20]][,3], col = "lightblue")
lines(res[[20]][,4], col = "darkgreen")

RETAILx <- data[["RETAILx"]]
date_ao <- 2007 + (11 - 1) / 12
y <- window(RETAILx, start = min(date_ao) - 2, end = max(date_ao)+2)
dates_studied <- time(y)[-(1:18)]
y_vintage <- lapply(dates_studied, window, x = y, start = NULL)
names(y_vintage) <- dates_studied
res <- lapply(y_vintage, all_filtered, 13, lc_f, filters_ao, date_ao)
plot(window(res[[20]][,1], start = 2006, end = 2008.5))
lines(res[[20]][,2], col = "orange")
lines(res[[20]][,3], col = "lightblue")
lines(res[[20]][,4], col = "darkgreen")
