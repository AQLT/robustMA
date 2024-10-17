library(rjd3filters)
source("0-functions.R")
lc_f <- lp_filter()
robust_ff <- readRDS("data/robust_ff.RDS")

confint <- confint_filter(y_vintage$"1998.58333333333", robust_ff[["ao"]]$t0)
confint2 <- confint_filter(y_vintage$"1998.58333333333", lc_f)
plot(confint2, plot.type = "single",
	 col = c("blue", "black", "black"),
	 lty = c(1, 2, 2))
lines(confint[,1], col = "red", lty = 1)

confint <- confint_filter(y_vintage$"1998.66666666667", robust_ff[["ao"]]$`t-1`)
confint2 <- confint_filter(y_vintage$"1998.66666666667", lc_f)
plot(confint2, plot.type = "single",
	 col = c("blue", "black", "black"),
	 lty = c(1, 2, 2))
lines(confint[,1], col = "red", lty = 1)

confint <- confint_filter(y_vintage$"1998.75", robust_ff[["ao"]]$`t-2`)
confint2 <- confint_filter(y_vintage$"1998.75", lc_f)
plot(confint2, plot.type = "single",
	 col = c("blue", "black", "black"),
	 lty = c(1, 2, 2))
lines(confint[,1], col = "red", lty = 1)

confint <- confint_filter(y_vintage$"1998.83333333333", robust_ff[["ao"]]$`t-3`)
confint2 <- confint_filter(y_vintage$"1998.83333333333", lc_f)
plot(confint2, plot.type = "single",
	 col = c("blue", "black", "black"),
	 lty = c(1, 2, 2))
lines(confint[,1], col = "red", lty = 1)

confint <- confint_filter(y_vintage$"1999", robust_ff[["ao"]]$t0)
confint2 <- confint_filter(y_vintage$"1999", lc_f)
plot(confint2, plot.type = "single",
	 col = c("blue", "black", "black"),
	 lty = c(1, 2, 2))
lines(confint[,1], col = "red", lty = 1)

plot(confint, plot.type = "single",
	 col = c("red", "black", "black"),
	 lty = c(1, 2, 2))
lines(confint2[,1], col = "blue", lty = 1)
data <- readRDS("data/data.rds")

# IPI voitures: 010768140
ipi_voitures <- data[["010768140"]]
date_ao <- c(1998 + (8-1)/12, 1999 + (8-1)/12)
y <- window(ipi_voitures, start = min(date_ao) - 2, end = max(date_ao)+2)
dates_studied <- time(y)[-(1:18)]
y_vintage <- lapply(dates_studied, window, x = y, start = NULL)
names(y_vintage) <- dates_studied
res = lapply(y_vintage, all_filtered, 13, lc_f, filters_ao, date_ao)

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
all_filtered(x, 13, lc_f, filters_ao, date_ao)
data_rob <- robfilter::robreg.filter(x, 13)
data_rob_level <- ts(data_rob$level,
					 start = start(x),
					 frequency = frequency(x))
lc_level <- x * lc_f
lc_out_level <- filtered_out(x, lc_f, filters_ao, date_ao)
res <- ts.union(x, lc_level, lc_out_level, data_rob_level)
colnames(res) <- c("y", "LC", "LC robust", colnames(data_rob_level))
res
plot()
lines(x*lc_f, col = "orange")
lines(x, col = "lightblue")

finite_filterslapply(-6:-1, function(t) filters_ao[[paste0("t",t)]][["0"]])
filters_ao$`t-5`$`0`
rev(filters_ao$`t-6`$`0`) * x
rev(filters_ao$`t-5`$`-1`) * x
rjd3filters:::filter_ma(x, rev(filters_ao$`t-6`$`0`))

lc_f * rep(x,2)

coefs = filters_ao$`t-5`$`-1`
coefs = filters_ao$`t-6`$`0`
function(x, coefs){
	# if (!is.moving_average(coefs)) {
	#   coefs <- moving_average(coefs, -abs(lags))
	# }
	lb <- lower_bound(coefs)
	ub <- upper_bound(coefs)

	if (length(x) < length(coefs))
		return(x * NA)

	jx <- .jcall(
		"jdplus/toolkit/base/core/data/DataBlock",
		"Ljdplus/toolkit/base/core/data/DataBlock;",
		"of",
		as.numeric(x)
	)
	out <- .jcall(
		"jdplus/toolkit/base/core/data/DataBlock",
		"Ljdplus/toolkit/base/core/data/DataBlock;",
		"of",
		.jarray(as.numeric(rep(NA, length(x) - length(coefs)+1)))
	)
	jfilter <- rjd3filters:::.ma2jd(coefs)
	.jcall(jfilter, "V", "apply",
		   jx, out)
	result <- .jcall(out, "[D", "toArray")
	result <- c(rep(NA, abs(min(lb, 0))),
				result,
				rep(NA, abs(max(ub, 0))))

	if (is.ts(x))
		result <- ts(result,start = start(x), frequency = frequency(x))
	result
}
