library(rjd3filters)
dir_exports <- file.path("results", "simul")
if (!dir.exists(dir_exports)) {
	dir.create(dir_exports, recursive = TRUE)
}

source("R/0-functions.R")
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

