library(rjd3filters)
dir_exports <- file.path("results", "AO")
if (!dir.exists(dir_exports)) {
	dir.create(dir_exports, recursive = TRUE)
}

source("0-functions.R")
lc_f <- lp_filter()
robust_filters <- readRDS("data/robust_filters.rds")




data <- readRDS("data/data.rds")

# IPI voitures
ipi_voitures <- data[["ipi_voitures"]]
#rjd3x13::regarima_fast(window(ipi_voitures, end = c(2019, 12)))
date_out <- c(1998 + (8-1)/12, 1999 + (8-1)/12)
y_vintage <- create_vintage(ipi_voitures, date_out)

res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ao"]], date_out)

saveRDS(list(data = res,
			 out = date_out),
		 file.path(dir_exports, "ipi_voitures98.rds")
		)

date_out <- c(2004 + (8-1)/12)
y_vintage <- create_vintage(ipi_voitures, date_out)
res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ao"]], date_out)
saveRDS(list(data = res,
			 out = date_out),
		file.path(dir_exports, "ipi_voitures2004.rds")
)

CE16OV <- data[["CE16OV"]]
date_out <- 2001 + (2 - 1) / 12
y_vintage <- create_vintage(CE16OV, date_out)
res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ao"]], date_out)
saveRDS(list(data = res,
			 out = date_out),
		file.path(dir_exports, "ce160v2001.rds")
)
# plot(window(res[[20]][,1], start = 2000))
# lines(res[[20]][,2], col = "orange")
# lines(res[[20]][,3], col = "lightblue")
# lines(res[[20]][,4], col = "darkgreen")

RETAILx <- data[["RETAILx"]]
date_out <- 2007 + (11 - 1) / 12
y_vintage <- create_vintage(RETAILx, date_out)
res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ao"]], date_out)
saveRDS(list(data = res,
			 out = date_out),
		file.path(dir_exports, "retailx2007.rds")
)

# plot(window(res[[20]][,1], start = 2006, end = 2008.5))
# lines(res[[20]][,2], col = "orange")
# lines(res[[20]][,3], col = "lightblue")
# lines(res[[20]][,4], col = "darkgreen")
