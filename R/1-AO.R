library(rjd3filters)
dir_exports <- file.path("results", "AO")
if (!dir.exists(dir_exports)) {
	dir.create(dir_exports, recursive = TRUE)
}

source("R/0-functions.R")
lc_f <- lp_filter()
robust_filters <- readRDS("data/robust_filters.rds")


data <- readRDS("data/data.rds")

# IPI voitures
ipi_voitures <- data[["ipi_voitures"]]
#rjd3x13::regarima_fast(window(ipi_voitures, end = c(2019, 12)))
date_out <- c(1998 + (8-1)/12, 1999 + (8-1)/12)
y_vintage <- create_vintage(ipi_voitures, date_out)

res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ao"]], date_out,
			  suff_robust_mm = " (AO)")

saveRDS(list(data = res,
			 out = date_out,
			 y = data[["ipi_voitures"]]),
		 file.path(dir_exports, "ipi_voitures98.rds")
		)

date_out <- c(2004 + (8-1)/12)
y_vintage <- create_vintage(ipi_voitures, date_out)
res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ao"]], date_out,
			  suff_robust_mm = " (AO)")
saveRDS(list(data = res,
			 out = date_out,
			 y = ipi_voitures),
		file.path(dir_exports, "ipi_voitures2004.rds")
)

##################################
##### Points de retournement #####
##################################

CE16OV <- data[["CE16OV"]]
date_out <- 2001 + (2 - 1) / 12
y_vintage <- create_vintage(CE16OV, date_out)
res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ao"]], date_out,
			  suff_robust_mm = " (AO)")
saveRDS(list(data = res,
			 out = date_out,
			 y = data[["CE16OV"]]),
		file.path(dir_exports, "ce16ov2001.rds")
)

RETAILx <- data[["RETAILx"]]
date_out <- 2007 + (11 - 1) / 12
y_vintage <- create_vintage(RETAILx, date_out)
res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["ao"]], date_out,
			  suff_robust_mm = " (AO)")
saveRDS(list(data = res,
			 out = date_out,
			 y = data[["RETAILx"]]),
		file.path(dir_exports, "retailx2007.rds")
)

