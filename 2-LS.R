library(rjd3filters)
dir_exports <- file.path("results", "LS")
if (!dir.exists(dir_exports)) {
	dir.create(dir_exports, recursive = TRUE)
}

source("0-functions.R")
lc_f <- lp_filter()

robust_filters <- readRDS("data/robust_filters.rds")

data <- readRDS("data/data.rds")

date_out <- 2010 + (6 - 1) / 12
ipi_petrole_brut <- data[["ipi_petrole_brut"]]
y_vintage <- create_vintage(ipi_petrole_brut, date_out)

res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ls"]], date_out)

saveRDS(list(data = res,
			 out = date_out),
		file.path(dir_exports, "ipi_petrole_brut10.rds")
)

res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["aoao"]], date_out)

saveRDS(list(data = res,
			 out = date_out),
		file.path(dir_exports, "ipi_petrole_brut10.rds")
)



