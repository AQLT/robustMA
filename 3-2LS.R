library(rjd3filters)
dir_exports <- file.path("results", "LSLS")
if (!dir.exists(dir_exports)) {
	dir.create(dir_exports, recursive = TRUE)
}

source("0-functions.R")

lc_f <- lp_filter()
robust_filters <- readRDS("data/robust_filters.rds")




data <- readRDS("data/data.rds")

# rjd3x13::regarima_fast(window(data$RETAILx,start = 1990, end = c(2019, 12)))
date_out <- c(2008 + (10-1)/12)
y_vintage <- create_vintage(data[["RETAILx"]], date_out)

res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["lsls"]], date_out)

saveRDS(list(data = res,
			 out = date_out),
		file.path(dir_exports, "retailx2008.RDS")
)
