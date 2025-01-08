library(rjd3filters)
dir_exports <- file.path("results", "simul")
if (!dir.exists(dir_exports)) {
	dir.create(dir_exports, recursive = TRUE)
}

source("0-functions.R")
lc_f <- lp_filter()
niveau_choc <- 0.1
robust_filters <- readRDS("data/robust_filters.rds")
# first_date <- 2020
first_date <- 2018
last_date <- c(2024, 12)
simul_ao_td0 <- simul_ls_td0 <- ts(1, start = first_date, end = last_date, frequency = 12)
date_out <- 2022

window(simul_ao_td0, start = date_out, end = date_out) <-
	window(simul_ao_td0, start = date_out, end = date_out) * (1 + niveau_choc)
window(simul_ls_td0, start = date_out) <-
	window(simul_ls_td0, start = date_out) * (1 + niveau_choc)

simul_ao_td1 <- simul_ao_td0 + (time(simul_ao_td0) - first_date)/frequency(simul_ao_td0)
simul_ls_td1 <- simul_ls_td0 + (time(simul_ls_td0) - first_date)/frequency(simul_ls_td0)

simul_ao_td2 <- -(time(simul_ao_td0) - date_out)^2
simul_ls_td2 <- simul_ao_td2 <- simul_ao_td2 + abs(min(simul_ao_td2)) + 1
window(simul_ao_td2, start = date_out, end = date_out) <-
	window(simul_ao_td2, start = date_out, end = date_out) * (1 + niveau_choc)
window(simul_ls_td2, start = date_out) <-
	window(simul_ls_td2, start = date_out) * (1 + niveau_choc)

################
### degree 0 ###
################

res <- lapply(
	create_vintage(simul_ao_td0, date_out),
	all_filtered, 13, lc_f, robust_filters[["ao"]], date_out,
	suff_robust_mm = " (AO)")

saveRDS(list(data = res,
			 out = date_out,
			 y = simul_ao_td0),
		file.path(dir_exports, "simul_ao_td0.RDS")
)

res <- lapply(
	create_vintage(simul_ls_td0, date_out),
	all_filtered, 13, lc_f, robust_filters[["ls"]], date_out,
	suff_robust_mm = " (LS)")

saveRDS(list(data = res,
			 out = date_out,
			 y = simul_ls_td0),
		file.path(dir_exports, "simul_ls_td0.RDS")
)


################
### degree 1 ###
################

res <- lapply(
	create_vintage(simul_ao_td1, date_out),
	all_filtered, 13, lc_f, robust_filters[["ao"]], date_out,
	suff_robust_mm = " (AO)")
res <- list(data = res,
			out = date_out,
			y = simul_ao_td1)
saveRDS(res,
		file.path(dir_exports, "simul_ao_td1.RDS")
)

res <- lapply(
	create_vintage(simul_ls_td1, date_out),
	all_filtered, 13, lc_f, robust_filters[["ls"]], date_out,
	suff_robust_mm = " (LS)")
res <- list(data = res,
	 out = date_out,
	 y = simul_ls_td1)
saveRDS(res,
		file.path(dir_exports, "simul_ls_td1.RDS")
)

################
### degree 2 ###
################

res <- lapply(
	create_vintage(simul_ao_td2, date_out),
	all_filtered, 13, lc_f, robust_filters[["ao"]], date_out,
	suff_robust_mm = " (AO)")
res <- list(data = res,
			out = date_out,
			y = simul_ao_td2)
saveRDS(res,
		file.path(dir_exports, "simul_ao_td2.RDS")
)

res <- lapply(
	create_vintage(simul_ls_td2, date_out),
	all_filtered, 13, lc_f, robust_filters[["ls"]], date_out,
	suff_robust_mm = " (LS)")
res <- list(data = res,
			out = date_out,
			y = simul_ls_td2)
saveRDS(res,
		file.path(dir_exports, "simul_ls_td2.RDS")
)

# simul_ao_t2 <- simul_ao_td1 +0.1((time(simul_ao_td0) - 2020))^2
#
# res <- lapply(
# 	create_vintage(simul_ao_t2, date_out),
# 	all_filtered, 13, lc_f, robust_filters[["ao"]], date_out)
# res <- list(data = res,
# 			out = date_out,
# 			y = simul_ao_td0)
# plot_y(res)
# y_as_plot <- FALSE
# add_y <- FALSE
# graph_est <- get_all_plots(res, y_as_plot = y_as_plot, add_y = add_y)
# graph_est$CLF <- NULL
# plot_y(res)
# patchwork::wrap_plots(graph_est, ncol = 3)
# graph_ci <- plot_confint(
# 	res,
# 	default_filter = lc_f,
# 	robust_f = robust_ff[["ao"]])
# patchwork::wrap_plots(graph_ci, ncol = 3)
x <- window(simul_ao_td2, end = c(2022, 2))
h = 6
lts_gen <- function(x, h = 6, degree = 2, method = c("lms", "lts")) {
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
lts_gen(x,method = "lms") - lts_gen(x,method = "lts")
plot(window(lts_gen(x,method = "lts",degree = 2), start = c(2021,7)))
lines(lts_gen(simul_ao_td2 ,method = "lts",degree = 2), col = "red")
plot(lts_gen(x,method = "lms"))
lines(lts_gen(simul_ao_td2 ,method = "lms"), col = "red")
