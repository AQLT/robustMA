sym_filter <- function(X = NULL, X_full = NULL, kernel = "Henderson") {
	kernel <- rjd3filters::get_kernel(kernel, horizon = 6)
	K <- diag(sapply(-6:6, function(i) kernel[i]))
	if (is.null(X_full))
		X_full <- cbind(rjd3filters::polynomial_matrix(l = -6, u = 6, d0 = 0, d1 = 3), X)
	e_1 <- rep(0, ncol(X_full))
	e_1[1] <- 1
	rjd3filters::moving_average(K %*% X_full %*% solve(t(X_full) %*% K %*% X_full, e_1),lags = -6)
}

reorder_out_filters <- function(filters) {
	obs_ao <- -12:0
	res <- lapply(obs_ao, function(t){
		focus_t_out <- seq.int(from = 6,
							   to = -6 + max(t + 6, 0))

		res <- lapply(focus_t_out, function(t_out){
			filters[[sprintf("t=%i", t_out)]][[sprintf("q=%i", min(abs(t - t_out), 6))]]
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

all_filtered <- function(x, robust_length = 13, default_filter, out_filter, date_out, ...) {
	data_rob <- robfilter::robreg.filter(x, robust_length)
	data_rob_level <- ts(data_rob$level,
						 start = start(x),
						 frequency = frequency(x))
	lc_level <- x * default_filter
	lc_out_level <- filtered_out(x, default_filter, out_filter, date_out)
	res <- ts.union(x, lc_level, lc_out_level, data_rob_level)
	colnames(res) <- c("y", "LC", "LC robust", colnames(data_rob_level))
	res
}

to_ff <- function(out_filters, default_filter) {
	lapply(out_filters, function(out_filter){
		last_out_ma <- max(as.numeric(names(out_filter)))
		if (last_out_ma < 0) {
			out_filter <- c(out_filter,
							default_filter@rfilters[rev(seq(6, by = -1, length.out = -last_out_ma))]
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
