library(rjd3filters)
source("0-functions.R")
dir_exports <- file.path("results", "AOLS")
if (!dir.exists(dir_exports)) {
	dir.create(dir_exports, recursive = TRUE)
}
lc_f <- lp_filter()
robust_filters <- readRDS("data/robust_filters.rds")

data <- readRDS("data/data.rds")

# rjd3x13::regarima_fast(window(data$immat, end = c(2019,12)))

date_out <- c(2018 + (8-1)/12)
y_vintage <- create_vintage(data[["immat"]], date_out)


res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["aols"]], date_out)

saveRDS(list(data = res,
			 out = date_out,
			 y = data[["immat"]]),
		file.path(dir_exports, "immat2018.RDS")
)

res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ls"]], date_out+1/12)
saveRDS(list(data = res,
			 out = date_out,
			 y = data[["immat"]]),
		file.path(dir_exports, "immat2018_ls.RDS")
)

res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ao"]], date_out)
saveRDS(list(data = res,
			 out = date_out,
			 y = data[["immat"]]),
		file.path(dir_exports, "immat2018_ao.RDS")
)
# all_est <- create_vintage_est(res)
# plot_est(all_est$`LC robust`)

