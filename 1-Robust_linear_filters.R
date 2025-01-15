library(rjd3filters)
source("0-functions.R")

gen_ls <- function(t, h = 6){
	if (t > -h & t <= h) {
		if (t <= 0) {
			X <- c(rep(-1,h+t), 0, rep(0, h-t))
		} else {
			X <- c(rep(0,h+t), 1, rep(1, h-t))
		}
		X <- matrix(X, ncol = 1)
	} else {
		X <- NULL
	}
	X
}
gen_ao <- function(t, h = 6){
	if (abs(t) <= h) {
		X <- c(rep(0, 6+t), 1, rep(0, 6-t))
		X <- matrix(X, ncol = 1)
	} else {
		X <- NULL
	}
	X
}
gen_ao_inv <- function(t, h = 6){
	X <- gen_ao(t, h)
	if (abs(t) <= h) {
		if (t <= 0) {
			X <- 1-X
		} else {
			X <- X
		}
		X <- matrix(X, ncol = 1)
	} else {
		X <- NULL
	}
	X
}
filters_ao <- lapply(6:-6, function(t) {
	X <- gen_ao(t)

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
filters_aoinv <- lapply(6:-6, function(t) {
	X <- gen_ao_inv(t)

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
names(filters_aoinv) <- paste0("t=", 6:-6)

filters_ls <- lapply(6:-6, function(t) {
	X <- gen_ls(t)
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
names(filters_ls) <- paste0("t=", 6:-6)

filters_aoao <- lapply(6:-7, function(t) {
	X <- cbind(gen_ao(t), gen_ao(t+1))
	sym <- sym_filter(X= X)
	res <- list("q=6" = sym)
	if (t == 6)
		return(res)
	all_q <- 5:max(t, 0)
	afilters <- lapply(all_q, function(q) {
		X <- X[,!apply(X[1:(6+q+1),,drop = FALSE]==0, 2,all)]
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
names(filters_aoao) <- paste0("t=", 6:-7)

filters_aoaoinv <- lapply(6:-7, function(t) {
	X <- cbind(gen_ao_inv(t), gen_ao_inv(t+1))
	sym <- sym_filter(X= X)
	res <- list("q=6" = sym)
	if (t == 6)
		return(res)
	all_q <- 5:max(t, 0)
	afilters <- lapply(all_q, function(q) {
		X <- X[,(!apply(X[1:(6+q+1),,drop = FALSE]==1, 2,all)) & !apply(X[1:(6+q+1),,drop = FALSE]==0, 2,all)]
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
names(filters_aoaoinv) <- paste0("t=", 6:-7)

filters_aoaoaoinv <- lapply(6:-8, function(t) {
	X <- cbind(gen_ao_inv(t), gen_ao_inv(t+1), gen_ao_inv(t+2))
	sym <- sym_filter(X= X)
	res <- list("q=6" = sym)
	if (t == 6)
		return(res)
	all_q <- 5:max(t, 0)
	afilters <- lapply(all_q, function(q) {
		X <- X[,(!apply(X[1:(6+q+1),,drop = FALSE]==1, 2,all)) & !apply(X[1:(6+q+1),,drop = FALSE]==0, 2,all)]
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
names(filters_aoaoaoinv) <- paste0("t=", 6:-8)

filters_aols <- lapply(6:-6, function(t) {
	# For t= -7 X = null
	X <- cbind(gen_ao(t), gen_ls(t+1))
	if (ncol(X) == 2 && length(unique(na.omit(X[,1]/X[,2]))) == 1)
		X <- X[,1,drop = FALSE]
	sym <- sym_filter(X= X)
	res <- list("q=6" = sym)
	if (t == 6)
		return(res)
	all_q <- 5:max(t, 0)
	afilters <- lapply(all_q, function(q) {
		X <- X[,!apply(X[1:(6+q+1),,drop = FALSE]==0, 2,all)]
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
names(filters_aols) <- paste0("t=", 6:-6)

filters_lsls <- lapply(6:-6, function(t) {
	# For t= -7 X = null
	X <- cbind(gen_ls(t), gen_ls(t+1))
	if (ncol(X) == 2 && length(unique(na.omit(X[,1]/X[,2]))) == 1)
		X <- X[,1,drop = FALSE]
	sym <- sym_filter(X= X)
	res <- list("q=6" = sym)
	if (t == 6 | is.null(X))
		return(res)
	all_q <- 5:max(t, 0)
	afilters <- lapply(all_q, function(q) {
		X <- X[,!apply(X[1:(6+q+1),,drop = FALSE]==0, 2,all)]
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
names(filters_lsls) <- paste0("t=", 6:-6)

robust_filters <- list(
	ao = filters_ao,
	aoinv = filters_aoinv,
	ls = filters_ls,
	aoao = filters_aoao,
	aoaoinv = filters_aoaoinv,
	aoaoaoinv = filters_aoaoaoinv,
	aols = filters_aols,
	lsls = filters_lsls
)
robust_filters <- lapply(robust_filters, reorder_out_filters)
default_filter <- lp_filter()
robust_ff <- lapply(robust_filters, to_ff, default_filter)
saveRDS(robust_filters, "data/robust_filters.RDS")
saveRDS(robust_ff, "data/robust_ff.RDS")
