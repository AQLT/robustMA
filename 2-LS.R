library(rjd3filters)
source("0-functions.R")
lc_f <- lp_filter()

filters_ls <- lapply(6:-6, function(t) {
	if (t != -6) {
		if (t <= 0) {
			X <- c(rep(-1,6+t), 0, rep(0, 6-t))
		} else {
			X <- c(rep(0,6+t), 1, rep(1, 6-t))

		}
	} else {
		X <- NULL
	}
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
filters_ls <- lapply(6:-6, function(t) {
	print(t)
	if (t != -6) {
		X <- c(rep(0,6+t), 1, rep(1, 6-t))
		U <- cbind(1-X, X)
		# 	X <- c(rep(-1,6+t), 0, rep(0, 6-t))
	} else {
		U <- polynomial_matrix(l = -6, d0 = 0, d1 = 0)
	}
	sym <- sym_filter(X_full= cbind(U, rjd3filters::polynomial_matrix(l = -6, u = 6, d0 = 1, d1 = 3)))
	res <- list("q=6" = sym)
	if (t == 6)
		return(res)
	all_q <- 5:max(t, 0)
	afilters <- lapply(all_q, function(q) {
		mmsre_filter(
			ref_filter = sym, q = q,
			delta = 2 / (sqrt(pi) * 3.5),
			U = U,
			Z = polynomial_matrix(l = -6, d0 = 1, d1 = 1),
		)
	})
	names(afilters) <- paste0("q=", all_q)
	res <- c(res, afilters)
	res
})
names(filters_ls) <- paste0("t=", 6:-6)

filters_ls <- reorder_out_filters(filters_ls)


data <- readRDS("data/data.rds")

date_ls <- 2010 + (6 - 1) / 12
y <- window(data[["010767578"]], start = min(date_ls) - 2, end = max(date_ls)+2)
dates_studied <- time(y)[-(1:18)]
y_vintage <- lapply(dates_studied, window, x = y, start = NULL)
names(y_vintage) <- dates_studied
res <- res2 <- lapply(y_vintage, all_filtered, 13, lc_f, filters_ls, date_ls)
AQLTools::graph_ts(res$"2011"[,1:3])

