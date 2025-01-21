sym_filter <- function(X = NULL, X_full = NULL, kernel = "Henderson", degree_max = 3) {
	kernel <- rjd3filters::get_kernel(kernel, horizon = 6)
	K <- diag(sapply(-6:6, function(i) kernel[i]))
	if (is.null(X_full))
		X_full <- cbind(rjd3filters::polynomial_matrix(l = -6, u = 6, d0 = 0, d1 = degree_max), X)
	e_1 <- rep(0, ncol(X_full))
	e_1[1] <- 1
	rjd3filters::moving_average(K %*% X_full %*% solve(t(X_full) %*% K %*% X_full, e_1),lags = -6)
}

reorder_out_filters <- function(filters, h = 6) {
	obs_ao <- seq.int(
		min(as.numeric(gsub("t=", "", names(filters)))) - h,
		0)
	res <- lapply(obs_ao, function(t){
		focus_t_out <- seq.int(from = h,
							   to = -h + max(t + h, 0))

		res <- lapply(focus_t_out, function(t_out){
			filters[[sprintf("t=%i", t_out)]][[sprintf("q=%i", min(abs(t - t_out), h))]]
		})
		names(res) <- (t - focus_t_out)
		res
	})
	names(res) <- sprintf("t%i", obs_ao)
	# names(res) <- gsub("t0", "t", names(res))
	res
}

filtered_out <- function(x, default_filter, out_filter, date_out) {
	filtered <- default_filter * x
	last_date <- time(x)[length(x)]
	for (t_out in date_out) {
		if (t_out <= last_date) {
			pos_out <- round((t_out - last_date) * frequency(x))
			min_date <- min(as.numeric(gsub("t", "", names(out_filter))))
			if (pos_out < min_date){
				focus_filters <- out_filter[[paste0("t", min_date)]]
				names(focus_filters) <- as.numeric(names(focus_filters)) + (pos_out  - min_date)
			} else {
				focus_filters <- out_filter[[paste0("t", pos_out)]]
			}

			for (t in as.numeric(names(focus_filters))) {
				window(
					filtered,
					start = last_date + t/frequency(x),
					end = last_date + t/frequency(x)
				) <-
					window(
						focus_filters[[as.character(t)]] * x,
						start = last_date + t/frequency(x),
						end = last_date + t/frequency(x)
					)
			}
		}
	}
	filtered
}

CLF <- list(
	c(-0.027, -0.007, 0.031, 0.067, 0.136, 0.188, 0.224, 0.188, 0.136, 0.067, 0.031, -0.007, -0.027),
	c(-0.026, -0.007, 0.030, 0.065, 0.132, 0.183, 0.219, 0.183, 0.132, 0.065, 0.030, -0.006),
	c(-0.026, -0.007, 0.030, 0.064, 0.131, 0.182, 0.218, 0.183, 0.132, 0.065, 0.031),
	c(-0.025, -0.004, 0.034, 0.069, 0.137, 0.187, 0.222, 0.185, 0.131, 0.064),
	c(-0.020, 0.005, 0.046, 0.083, 0.149, 0.196, 0.226, 0.184, 0.130),
	c(0.001, 0.033, 0.075, 0.108, 0.167, 0.205, 0.229, 0.182),
	c(0.045, 0.076, 0.114, 0.134, 0.182, 0.218, 0.230)
)
CLF <- rjd3filters::finite_filters(CLF)
CLF_CN <- lapply(0:6, function(i) {
	sfilter <- CLF@sfilter
	if (i==0)
		return(coef(sfilter))
	coef_cut <- rev(rev(coef(sfilter))[seq.int(-1, by = -1, length.out = i)])
	coef_norm <- coef_cut / sum(coef_cut)
	coef_norm
	})
CLF_CN <- rjd3filters::finite_filters(CLF_CN)

all_filtered <- function(
		x, robust_length = 13, default_filter, out_filter, date_out,
		order_robust = c("MED", "RM", "LMS", "LTS", "LQD", "DR"),
		extend_series_robust = TRUE,
		suff_robust_mm = "",
		d2robustM = TRUE,
		...) {

	h <- trunc((robust_length - 1) / 2)
	if (extend_series_robust) {
		x_rob <- window(x,
						start = time(x)[1] - h / frequency(x),
						end = time(x)[length(x)] + h / frequency(x),
						extend = TRUE
						)
	} else {
		x_rob <- x
	}
	capture.output(data_rob <- robfilter::robreg.filter(x_rob, robust_length))
	data_rob_level <- ts(data_rob$level[,order_robust],
						 start = start(x_rob),
						 frequency = frequency(x))
	data_rob_level <- window(data_rob_level, start = start(x),
							 end = end(x))
	lc_level <- x * default_filter
	CLF_level <- x * CLF
	CLF_CN_level <- x * CLF_CN
	lc_out_level <- filtered_out(x, default_filter, out_filter, date_out)
	res <- ts.union(x, lc_level, lc_out_level, CLF_level, CLF_CN_level, data_rob_level)
	colnames(res) <- c("y", "Musgrave", paste0("Musgrave robuste", suff_robust_mm),
					   "CLF et ALF", "CLF (coupe et normalise)", colnames(data_rob_level))
	if (d2robustM) {
		res2 <- ts.union(res, lqs_d2(x, h = h,method = "lms"), lqs_d2(x, h = h,method = "lts"))
		colnames(res2) <- c(colnames(res), "LMS tendance d2", "LTS tendance d2")
		res <- res2
	}
	res
}

to_ff <- function(out_filters, default_filter) {
	lapply(out_filters, function(out_filter){
		last_out_ma <- max(as.numeric(names(out_filter)))
		if (last_out_ma < 0) {
			out_filter <- c(out_filter,
							default_filter@rfilters[rev(seq(6, by = -1, length.out = min(-last_out_ma, 6)))]
			)
		}
		nrep <- length(out_filter) - 6
		finite_filters(
			moving_average(c(rep(0, nrep), coef(default_filter@sfilter), rep(0, nrep)),
						   lags = -6 - nrep),
			rfilters =  out_filter,
			lfilters = c(
				default_filter@lfilters,
				rep(list(default_filter@sfilter), nrep)
			)
		)
	})
}

create_vintage <- function(x, date_out, ny_before = 2, ny_after = ny_before, nb_removed = 18) {
	y <- window(
		x,
		start = max(min(date_out) - ny_before, time(x)[1]),
		end = min(max(date_out) + ny_after, time(x)[length(x)])
	)
	dates_studied <- time(y)[-(1:nb_removed)]
	y_vintage <- lapply(dates_studied, window, x = y, start = NULL)
	names(y_vintage) <- dates_studied
	y_vintage
}

create_vintage_est <- function(res) {
	if (!is.null(res$out))
		res <- res$data
	all_est <- lapply(colnames(res[[1]]), function(col){
		table <- do.call(cbind, lapply(res, `[`,,col))
		colnames(table) <- as.character(zoo::as.yearmon(as.numeric(colnames(table))))
		table
	})
	names(all_est) <- colnames(res[[1]])
	all_est
}

lqs_d2 <- function(x, h = 6, degree = 2, method = c("lms", "lts")) {
	method <- match.arg(method)
	res <- x * NA
	first_date <- time(x)[1]
	last_date <- time(x)[length(x)]
	delta_t <- deltat(res)
	freq <- frequency(res)
	for (date in time(res)) {
		try({
			fd_est <- max(date - h * delta_t, first_date)
			ld_est <- min(date + h * delta_t, last_date)
			X = rjd3filters::polynomial_matrix(l = round((fd_est - date) * freq),
											   u =round((ld_est - date) * freq),
											   d0 = 1, d1 = degree)
			reg_est <- MASS::lqs(X, window(x, start = fd_est, end = ld_est),
								 method = method)
			window(res, start = date, end = date) <- coef(reg_est)[1]
		})
	}
	res
}

