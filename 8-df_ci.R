library(rjd3filters)
x <- retailsa$DrinkingPlaces
coef_lp <- lp_filter(6)
n = length(x)
coef = coef_lp@rfilters[[2]]
coef = coef_lp@sfilter

p <- abs(lower_bound(coef))
f <- upper_bound(coef)

mb_delta <- microbenchmark::microbenchmark(
	act = {
		L =matrix(0,ncol = n, nrow = n)
		I = diag(x=1, nrow = n)
		for (i in seq(1-lower_bound(coef), n - upper_bound(coef))){
			L[i,seq(i + lower_bound(coef), length.out = length(coef))] = coef(coef)
		}
		if (lower_bound(coef) < 0){
			I[1:(-lower_bound(coef)),] <- 0
		}
		if (upper_bound(coef) > 0){
			I[(nrow(I)-upper_bound(coef)+1):nrow(I),] <- 0
		}
		Delta = t(I - L) %*% (I - L)
		sum(diag(Delta))^2 / sum(diag(Delta %*% Delta))
	},
	new = {
		value_coef <- coefficients(coef)
		coef0 <- value_coef["t"]
		num <- ((n - (p + f))*(1- 2 * coef0 + sum(value_coef^2)))^2
		value_coef <- - value_coef
		value_coef["t"] <- 1 + value_coef["t"]
		mat_coefs <- do.call(cbind, lapply(0:(p + f), function(n_0) {
			c(rep(0, n_0), value_coef[seq(1, length.out = length(value_coef) - n_0)])
		}))
		stats <- value_coef %*% mat_coefs
		stats <- stats ^ 2
		stats[-1] <- stats[-1] * 2
		denum <- sum((n-(p+f) - seq(0, length.out = length(stats))) * stats)
		num / denum
	},
	simplified = {
		coef0 <- coefficients(coef)["t"]
		df <- (n - (p + f))*(1- 2 * coef0 + sum(coefficients(coef)^2))
	},
	times = 1000
)
saveRDS(
	mb_delta,
	file = "data/microbenchmark_df_ci.RDS"
)
mb_delta <- readRDS("data/microbenchmark_df_ci.RDS")
mb_delta
ggplot2::autoplot(mb_delta)

