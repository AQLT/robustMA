library(rjd3filters)
dir_exports <- file.path("results", "LSLS")
if (!dir.exists(dir_exports)) {
	dir.create(dir_exports, recursive = TRUE)
}

source("R/0-functions.R")

lc_f <- lp_filter(ic = 3.5)
robust_filters <- readRDS("data/robust_filters.rds")




data <- readRDS("data/data.rds")

# rjd3x13::regarima_fast(window(data$RETAILx,start = 1990, end = c(2019, 12)))
date_out <- c(2008 + (10-1)/12)
y_vintage <- create_vintage(data[["RETAILx"]], date_out)

res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["lsls"]], date_out,
			  suff_robust_mm = " (LS-LS)")

saveRDS(list(data = res,
			 out = date_out,
			 y = data[["RETAILx"]]),
		file.path(dir_exports, "retailx2008.RDS")
)


y <- data$CE16OV
date_out <- 2020 + (3 - 1) / 12

res <- lapply(
	create_vintage(y, date_out),
	all_filtered, 13, lc_f, robust_filters[["lsls"]], date_out,
	suff_robust_mm = " (LS-LS)")
res <- list(data = res,
			out = date_out,
			y = y)
saveRDS(res,
		file.path(dir_exports, "ce16ovcovid.RDS")
)
# graph_est <- get_all_plots(res, y_as_plot = y_as_plot, add_y = TRUE)
# graph_est$CLF <- NULL
# patchwork::wrap_plots(graph_est[1:2])
# patchwork::wrap_plots(graph_est, ncol = 3)
# graph_y <- plot_y(res, n_xlabel = 6)
# graph_ci <- plot_confint(
# 	res,
# 	default_filter = lc_f,
# 	robust_f = list("MM robuste LS-LS" = robust_ff[["lsls"]],
# 					"MM robuste AO-AO (TC)" = robust_ff[["aoaotc"]]))
#
# apply_consistent_y_lims(patchwork::wrap_plots(graph_ci, ncol = 3)) +
# 	plot_layout(guides = 'collect', axes = "collect") &
# 	theme(legend.position='bottom')


y <- data$ipi_manuf
date_out <- 2020 + (3 - 1) / 12

res <- lapply(
	create_vintage(y, date_out),
	all_filtered, 13, lc_f, robust_filters[["lsls"]], date_out,
	suff_robust_mm = " (LS-LS)")
# res <- lapply(
# 	create_vintage(y, date_out),
# 	all_filtered, 13, lc_f, robust_filters[["lsls"]], date_out, d2robustM = TRUE,
# 	suff_robust_mm = " (LS-LS)")
res <- list(data = res,
			out = date_out,
			y = y)
saveRDS(res,
		file.path(dir_exports, "ipi_manufcovid.RDS")
)

date_out <- 2020 + (3 - 1) / 12
y_vintage <- create_vintage(data[["RETAILx"]], date_out)

res <- lapply(y_vintage, all_filtered, 13, lc_f, robust_filters[["lsls"]], date_out)
res <- list(data = res,
			out = date_out,
			y = data[["RETAILx"]])
saveRDS(res,
		file.path(dir_exports, "retailxcovid.RDS")
)
# graph_est <- get_all_plots(res, y_as_plot = y_as_plot, add_y = F,share_y_lim = FALSE)
# graph_est$CLF <- NULL
# graph_y <- plot_y(res, n_xlabel = 6)
# graph_ci <- plot_confint(
# 	res,
# 	default_filter = lc_f,
# 	robust_f = list("MM robuste LS" = robust_ff[["ls"]],
# 					"MM robuste LSLS" = robust_ff[["lsp1"]]))
#
# patchwork::wrap_plots(graph_est[1:2])
# patchwork::wrap_plots(graph_est, ncol = 3)
# apply_consistent_y_lims(patchwork::wrap_plots(graph_ci, ncol = 3)) +
# 	plot_layout(guides = 'collect', axes = "collect") &
# 	theme(legend.position='bottom')
