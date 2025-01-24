library(rjd3filters)
dir_exports <- file.path("results", "LS")
if (!dir.exists(dir_exports)) {
	dir.create(dir_exports, recursive = TRUE)
}

source("R/0-functions.R")
lc_f <- lp_filter()

robust_filters <- readRDS("data/robust_filters.rds")

data <- readRDS("data/data.rds")

date_out <- 2010 + (6 - 1) / 12
ipi_petrole_brut <- data[["ipi_petrole_brut"]]
# série plus courte car pas le même schéma en début de période
# cela permet d'éviter d'augmenter trop la variance
ipi_petrole_brut <- window(ipi_petrole_brut, start = 2001)
y_vintage <- create_vintage(ipi_petrole_brut, date_out)

res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ls"]], date_out,
			  suff_robust_mm = " (LS)")

saveRDS(list(data = res,
			 out = date_out,
			 y = ipi_petrole_brut),
		file.path(dir_exports, "ipi_petrole_brut10.rds")
)
