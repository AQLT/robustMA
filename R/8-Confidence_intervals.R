library(rjd3filters)
source("R/0-functions.R")
options(is_french = TRUE)
source("R/0-functions-plot.R")
lc_f <- lp_filter(ic = 3.5)
robust_ff <- readRDS("data/robust_ff.rds")


patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "AO", "ipi_voitures98.rds")),
		out = 1998 + (8-1)/12, default_filter = lc_f,
		robust_f = robust_ff[["ao"]],
		nest = 6),
	ncol = 3)
patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "AO", "ipi_voitures98.rds")),
		out = 1998 + (8-1)/12, default_filter = lc_f,
		robust_f = robust_ff[["ao"]],
		nest = 6,gaussian_distribution = TRUE),
	ncol = 3)

patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "AO", "ipi_voitures2004.rds")),
		out = 2004 + (8-1)/12, default_filter = lc_f,
		robust_f = robust_ff[["ao"]],
		nest = 6),
	ncol = 3)

patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "AO", "ipi_voitures2004.rds")),
		out = 2004 + (8-1)/12, default_filter = lc_f,
		robust_f = robust_ff[["ls"]],
		nest = 6),
	ncol = 3)

patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "LS", "ipi_petrole_brut10.rds")),
		out = 2010 + (6 - 1) / 12, default_filter = lc_f,
		robust_f = robust_ff[["ls"]],
		nest = 6),
	ncol = 3)

patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "LSLS", "retailx2008.RDS")),
		out = 2008 + (10-1)/12, default_filter = lc_f,
		robust_f = robust_ff[["lsls"]],
		nest = 6),
	ncol = 3)

patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "aols", "immat2018_ao.RDS")),
		out = 2018 + (8-1)/12, default_filter = lc_f,
		robust_f = robust_ff[["aols"]],
		nest = 6),
	ncol = 3)
patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "aols", "immat2018_ao.RDS")),
		out = 2018 + (8-1)/12, default_filter = robust_ff[["ao"]],
		robust_f = robust_ff[["aols"]],
		nest = 6),
	ncol = 3)

patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "aols", "immat2018_ao.RDS")),
		out = 2018 + (8-1)/12, default_filter = lc_f,
		robust_f = robust_ff[["ao"]],
		nest = 6),
	ncol = 3)

