library(rjd3filters)
dir_exports <- file.path("results", "simul")
if (!dir.exists(dir_exports)) {
	dir.create(dir_exports, recursive = TRUE)
}

source("0-functions.R")
lc_f <- lp_filter()
niveau_choc <- 0.1
robust_filters <- readRDS("data/robust_filters.rds")
simul_ao <- simul_ls <- ts(1, start =2020, end = c(2024, 12), frequency = 12)
date_out <- 2022
window(simul_ao, start = date_out, end = date_out) <-
	window(simul_ao, start = date_out, end = date_out) * (1 + niveau_choc)
window(simul_ls, start = date_out) <-
	window(simul_ls, start = date_out) * (1 + niveau_choc)

res <- lapply(
	create_vintage(simul_ao, date_out),
	all_filtered, 13, lc_f, robust_filters[["ao"]], date_out)

saveRDS(list(data = res,
			 out = date_out,
			 y = simul_ao),
		file.path(dir_exports, "simul_ao.RDS")
)

res <- lapply(
	create_vintage(simul_ls, date_out),
	all_filtered, 13, lc_f, robust_filters[["ls"]], date_out)

saveRDS(list(data = res,
			 out = date_out,
			 y = simul_ls),
		file.path(dir_exports, "simul_ls.RDS")
)
