library(rjd3filters)
x <- retailsa$DrinkingPlaces
coef_lp <- lp_filter(6)
n = length(x)
coef = coef_lp@rfilters[[2]]
rjd3filters:::df_var(n = n, coef = coef, exact_df = TRUE)
rjd3filters:::df_var(n = n, coef = coef, exact_df = FALSE)


# mat_coefs <- do.call(cbind, lapply(0:(p + f), function(n_0) {
# 	c(rep(0, n_0), coef(coef)[seq(1, length.out = length(coef) - n_0)])
# }))
# stats <- coef(coef) %*% mat_coefs
# # stats <- sapply(0:(p + f), function(n_0) {
# # 	sum(mat_coefs[,1+n_0] %*% mat_coefs[,seq(1, length.out = n_0+1)])
# # })
# stats <- stats ^ 2
# stats[-1] <- stats[-1] * 2
# sum((n-(p+f) - seq(0, length.out = length(stats))) * stats)
# L =matrix(0,ncol = n, nrow = n)
# for (i in seq(1-lower_bound(coef), n - upper_bound(coef))){
# 	L[i,seq(i + lower_bound(coef), length.out = length(coef))] = coef(coef)
# }
# sum(diag((L %*% t(L)) %*% (L %*% t(L))))

p <- abs(lower_bound(coef))
f <- upper_bound(coef)
mb_h <- microbenchmark::microbenchmark(
	act = {
		L =matrix(0,ncol = n, nrow = n)
		for (i in seq(1-lower_bound(coef), n - upper_bound(coef))){
			L[i,seq(i + lower_bound(coef), length.out = length(coef))] = coef(coef)
		}
		sum(diag((L %*% t(L)) %*% (L %*% t(L))))
	},
	new = {
		value_coef <- coefficients(coef)
		mat_coefs <- do.call(cbind, lapply(0:(p + f), function(n_0) {
			c(rep(0, n_0), value_coef[seq(1, length.out = length(value_coef) - n_0)])
		}))
		stats <- value_coef %*% mat_coefs
		# stats <- sapply(0:(p + f), function(n_0) {
		# 	sum(mat_coefs[,1+n_0] %*% mat_coefs[,seq(1, length.out = n_0+1)])
		# })
		stats <- stats ^ 2
		stats[-1] <- stats[-1] * 2
		sum((n-(p+f) - seq(0, length.out = length(stats))) * stats)
	},
	simplified = {
		coef0 <- coefficients(coef)["t"]
		df <- (n - (upper_bound(coef) - lower_bound(coef)))*(1- 2 * coef0 + sum(coefficients(coef)^2))
	},
	times = 1000
)
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
mb_h # L
mb_delta # df final
saveRDS(
	list(mb_h = mb_h,
		 mb_delta = mb_delta),
	file = "data/microbenchmark_df_ci.RDS"
)
# 124642.4095 / c(426.5425, 19.1825)
# df_var(n = n, coef = coef@sfilter, exact_df = TRUE)
# microbenchmark::microbenchmark(
# 	rjd3filters:::df_var(n = n, coef = coef_lp@sfilter, exact_df = FALSE),
# 	rjd3filters:::df_var(n = n, coef = coef_lp@sfilter, exact_df = TRUE),
# 	formule = {stats <- 0
# 	for (i in 0:(f+p)) {
# 		somme_tmp <- 0
# 		for (l in 0:i) {
# 			somme_tmp <- somme_tmp + coef(coef)[1+l] * coef(coef)[p+f+l-i+1]
# 		}
# 		stats <- stats + somme_tmp^2
# 	}
# 	}
# )
#
# sum
# stats2 <- stats
#
# stats <- 0
# for (i in 0:(f+p-1)) {
# 	somme_tmp <- 0
# 	for (l in 0:i) {
# 		somme_tmp <- somme_tmp + coef(coef)[1+l] * coef(coef)[p+f+l-i+1]
# 	}
# 	stats <- stats + somme_tmp^2
# }
# stats1 <- stats
# 2 * stats
#
# (sum(coefficients(coef)^2)^2 + 2 * stats1) * (n-p-f)
# (2*stats2 - sum(coefficients(coef)^2)^2) * (n-p-f)
#
# unique(diag((L %*% t(L)) %*% (L %*% t(L))))
# (L %*% t(L))[7,] == (L %*% t(L))[,7]
# df_var <- function(n, coef, exact_df = FALSE) {
# 	if (!exact_df){
# 		coef0 <- coefficients(coef)["t"]
# 		df <- (n - (upper_bound(coef) - lower_bound(coef)))*(1- 2 * coef0 + sum(coefficients(coef)^2))
# 		return(df)
# 	}
# 	L =matrix(0,ncol = n, nrow = n)
# 	I = diag(x=1, nrow = n)
# 	for (i in seq(1-lower_bound(coef), n - upper_bound(coef))){
# 		L[i,seq(i + lower_bound(coef), length.out = length(coef))] = coef(coef)
# 	}
# 	if (lower_bound(coef) < 0){
# 		I[1:(-lower_bound(coef)),] <- 0
# 	}
# 	if (upper_bound(coef) > 0){
# 		I[(nrow(I)-upper_bound(coef)+1):nrow(I),] <- 0
# 	}
# 	Delta = t(I - L) %*% (I - L)
# 	df <- sum(diag(Delta))^2 / sum(diag(Delta %*% Delta))
# 	return(df)
# }
