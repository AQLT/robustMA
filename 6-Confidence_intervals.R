library(rjd3filters)
source("0-functions.R")
source("0-functions-plot.R")
lc_f <- lp_filter()
robust_ff <- readRDS("data/robust_ff.rds")

res <- readRDS(file.path("results", "AO", "ipi_voitures90.rds"))



patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "AO", "ipi_voitures90.rds"))$data,
		out = 1998 + (8-1)/12, default_filter = lc_f,
		robust_f = robust_ff[["ao"]],
		nest = 6),
	ncol = 3)



patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "AO", "ipi_voitures2004.rds"))$data,
		out = 2004 + (8-1)/12, default_filter = lc_f,
		robust_f = robust_ff[["ao"]],
		nest = 6),
	ncol = 3)

patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "AO", "ipi_voitures2004.rds"))$data,
		out = 2004 + (8-1)/12, default_filter = lc_f,
		robust_f = robust_ff[["ls"]],
		nest = 6),
	ncol = 3)

patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "LS", "ipi_petrole_brut10.rds"))$data,
		out = 2010 + (6 - 1) / 12, default_filter = lc_f,
		robust_f = robust_ff[["ls"]],
		nest = 6),
	ncol = 3)

patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "LSLS", "retailx2008.RDS"))$data,
		out = 2008 + (10-1)/12, default_filter = lc_f,
		robust_f = robust_ff[["lsls"]],
		nest = 6),
	ncol = 3)

patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "aols", "immat2018_ao.RDS"))$data,
		out = 2018 + (8-1)/12, default_filter = lc_f,
		robust_f = robust_ff[["aols"]],
		nest = 6),
	ncol = 3)
patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "aols", "immat2018_ao.RDS"))$data,
		out = 2018 + (8-1)/12, default_filter = robust_ff[["ao"]],
		robust_f = robust_ff[["aols"]],
		nest = 6),
	ncol = 3)

patchwork::wrap_plots(
	plot_confint(
		readRDS(file.path("results", "aols", "immat2018_ao.RDS"))$data,
		out = 2018 + (8-1)/12, default_filter = lc_f,
		robust_f = robust_ff[["ao"]],
		nest = 6),
	ncol = 3)

