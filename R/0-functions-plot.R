plot_est <- function (data, vline =NULL, titre = NULL, sous_titre = NULL, limits_y = NULL, outDec = ".",
					  extra_series = NULL, colour.extra_series = "darkgreen") {
	x_lab = y_lab= NULL
	n_xlabel = 5 ;n_ylabel = 4;
	x_breaks <- time(data)[seq.int(1, length((time(data))), by = length((time(data))) %/% n_xlabel)]


	dataGraph <- format_data_plot(data)
	data_legend <- dataGraph |>
		dplyr::group_by(variable) |>
		dplyr::filter(date == max(date)) |>
		data.frame()
	p <- ggplot2::ggplot(data = dataGraph)
	if (!is.null(vline)) {
		for (xintercept in vline) {
			p <- p + ggplot2::geom_vline(xintercept = xintercept, linetype = "dotted", alpha = 0.5)
		}
	}
	if (!is.null(extra_series))
		p <- p + ggplot2::geom_line(data = format_data_plot(extra_series),
									ggplot2::aes(x = date, y = value),
									linewidth = 0.7, alpha = 0.5,
									colour = colour.extra_series)

	p <- p +
		ggplot2::geom_line(mapping = ggplot2::aes(x = date, y = value, group = variable,
												  colour = variable), linewidth = 0.7) +
		ggplot2::labs(title = titre, subtitle = sous_titre, x = x_lab,
					  y = y_lab) +
		ggplot2::scale_x_continuous(breaks = x_breaks,
									labels = zoo::as.yearmon) +
		ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = n_ylabel),
									labels = function(x) format(x, decimal.mark = outDec)) +
		ggplot2::coord_cartesian(ylim = limits_y) +
		ggplot2::theme_bw()
	p +
		ggplot2::geom_text(ggplot2::aes(x = date, y = value, label =variable, colour = variable), data = data_legend,
						   check_overlap = TRUE, hjust = 0, nudge_x = 0.01,
						   size = 2) +
		ggplot2::theme(legend.position = "none",
					   plot.title = ggplot2::element_text(hjust = 0.5),
					   panel.grid.minor.x = ggplot2::element_blank(),
					   panel.grid.major.x = ggplot2::element_blank(),
					   axis.text.x = ggplot2::element_text(size=8, angle=20,
					   									vjust=1.1, hjust=1),
					   axis.text.y = ggplot2::element_text(size=8),
					   plot.subtitle = ggplot2::element_text(hjust = 0.5,
					   									  size=10,face="italic"))
}
format_data_plot  <- function(data){
	time <- time(data)
	freq <- frequency(data)
	dataGraph <- data.frame(cbind(time, data))
	colnames(dataGraph) <- c("date", colnames(data))
	na.omit(reshape2::melt(dataGraph, id = "date"))
}

plot_confint <- function(data, out = NULL, default_filter, robust_f,
						 nb_dates_before = 8, nest = 6, add_y = FALSE,
						 gaussian_distribution = FALSE, exact_df = TRUE,
						 names_robust_f = NULL,
						 show.legend = FALSE,
						 same_x_limit = TRUE,
						 date_outlier = TRUE,
						 n_xlabel = 5) {
	if (is.null(out)) {
		out <- data$out
	}
	y_long <- NULL
	if (!is.null(data$data)) {
		y_long <- data$y
		data <- data$data
	}

	first_date <- min(out) - nb_dates_before * deltat(data[[1]])
	lagest <- seq(0, by = 1, length.out = nest)

	if (rjd3filters::is.finite_filters(robust_f[[1]])) {
		robust_f <- list("Robust" = robust_f)
	}
	if (is.null(names_robust_f))
		names_robust_f <- names(robust_f)
	if (same_x_limit) {
		limits_x <- c(first_date - 10^-9,
					  max(out) + (nest - 1) * deltat(data[[1]]) +  10^-9)

		dates <- seq(from = first_date, to = max(out) + (nest - 1) * deltat(data[[1]]),
					 by = deltat(data[[1]]))
		x_breaks <- dates[seq.int(1, length((dates)), by = length((dates)) %/% n_xlabel)]
		if (date_outlier)
			x_breaks <- unique(c(x_breaks, out))
	} else {
		limits_x <- NULL
	}
	all_plots <- lapply(lagest, function(i){
		y <- data[[which(round(out,3) == round(as.numeric(names(data)),3)) + i]][,"y"]
		if (is.null(y_long)) {
			y_confint <- y
		} else {
			# we use all the data to compute the confindence interval to avoid begin influence
			# by the length of the data
			y_confint <- window(y_long, end = end(y))
		}
		est_robust <- do.call(ts.union, lapply(robust_f, function(x){
			y * x[[sprintf("t%i", -i)]]
		}))
		# confint_robust <- confint_filter(y_confint, robust_f[[sprintf("t%i", -i)]], gaussian_distribution = gaussian_distribution, exact_df = exact_df)
		confint_default <- confint_filter(y_confint, lc_f, gaussian_distribution = gaussian_distribution, exact_df = exact_df)
		data_plot <- ts.union(confint_default, est_robust, y)
		data_plot <- window(data_plot, start = first_date)
		colnames(data_plot) <- c("Musgrave", "Confint_m", "Confint_p", names_robust_f,"y")
		if (!same_x_limit) {
			dates <- window(time(data_plot), start = first_date)
			x_breaks <- dates[seq.int(1, length((dates)), by = length((dates)) %/% n_xlabel)]
		}
		data_plot_wide <- data.frame(date = as.numeric(time(data_plot)), data_plot)
		data_plot_long <- format_data_plot(data_plot)
		data_plot_long <- data_plot_long[data_plot_long$variable %in% c(colnames(data_plot)[1], names_robust_f), ]
		data_plot_long$variable <- factor(data_plot_long$variable, levels = c(colnames(data_plot)[1], names_robust_f), ordered = TRUE)
		p <- ggplot2::ggplot(data = data_plot_wide, ggplot2::aes(x = date)) +
			ggplot2::geom_ribbon(
				ggplot2::aes(ymin = Confint_m, ymax = Confint_p, fill = "Intervalle de confiance 95 %")) +
			scale_fill_manual(values=c("grey")) +
			ggplot2::geom_vline(xintercept = out,linetype= 2, alpha = 0.5)
			# ggplot2::geom_line(ggplot2::aes(y = Default), col = "blue")
		if (add_y) {
			p <- p +
				ggplot2::geom_line(ggplot2::aes(y = y), col = "black")
		}
		p <- suppressMessages({
			 p +
			ggplot2::geom_line(data = data_plot_long,
							   ggplot2::aes(y = value, color = variable)) +
			ggplot2::labs(x = NULL, y = NULL,
						  title = as.character(zoo::as.yearmon(out + i*deltat(y))),
						  color = NULL,
						  fill = NULL) +
			ggplot2::scale_color_manual(values = c("blue", "red", "darkgreen", "orange")) +
			ggplot2::theme_bw() +
			ggplot2::scale_x_continuous(breaks = x_breaks,
										labels = zoo::as.yearmon,
										limits = limits_x) +
			# ggplot2::coord_cartesian(xlim = limits_x,
			# 						 default = TRUE) +
			ggplot2::theme(
				plot.title = ggplot2::element_text(hjust = 0.5),
				panel.grid.minor.x = ggplot2::element_blank(),
				panel.grid.major.x = ggplot2::element_blank(),
				axis.text.x = ggplot2::element_text(size=8, angle=20,
													vjust=1.1, hjust=1),
				axis.text.y = ggplot2::element_text(size=8)
			)
		})
		if (!show.legend)
			p <- p + ggplot2::theme(legend.position = "none")
		p

	})
	names(all_plots) <- as.character(zoo::as.yearmon(out + lagest*deltat(data[[1]])))
	all_plots
}

get_all_plots <- function(
		res,
		out = NULL,
		nb_est = 10, nb_dates_before = 6,
		vline = TRUE,
		add_y = FALSE,
		y_as_plot = FALSE,
		share_y_lim = TRUE,
		multiple_vline_out = FALSE,
		modeles_retenus = NULL
		){
	if (is.null(out)) {
		out <- res$out
	}
	if (length(out) > 1 & !multiple_vline_out) {
		all_plots <- lapply(out, get_all_plots, res = res, nb_est = nb_est,
							nb_dates_before = nb_dates_before, vline = vline, add_y = add_y,
							y_as_plot = y_as_plot, share_y_lim = share_y_lim,
							modeles_retenus = modeles_retenus)
		names(all_plots) <- out
		return(all_plots)
	}
	all_est <- create_vintage_est(res)
	if(!is.null(modeles_retenus)) {
		if (is.numeric(modeles_retenus)) {
			mod <- all_est[modeles_retenus + 1]
		} else {
			mod <- all_est[modeles_retenus]
		}
		all_est <- c(all_est["y"], mod)
	}
	y <- all_est$y[,ncol(all_est$y)]
	all_est$y <- NULL
	cols <- seq.int(
		which(!is.na(window(all_est[[1]], start = out[1], end = out[1]))),
		length.out = nb_est)
	data_plots <- lapply(all_est, `[`,, cols)
	data_plots <- lapply(data_plots, window,
						 start  = out[1] - nb_dates_before * deltat(y),
						 end  = out[1] + (nb_est - 1) * deltat(y))
	y <- window(y, start = start(data_plots[[1]]), end = end(data_plots[[1]]))
	if (vline) {
		vline <- out
	} else {
		vline <- NULL
	}
	if(add_y) {
		extra_series <- y
	} else {
		extra_series <- NULL
	}
	if (share_y_lim & !add_y)  {
		limits_y <- range(unlist(sapply(data_plots[grep("^CLF$", names(data_plots), invert = TRUE)],
										range,na.rm = TRUE)),
						  na.rm = TRUE)
	} else {
		limits_y <- NULL
	}

	all_plots <- lapply(names(data_plots), function(x){
		plot_est(data_plots[[x]], titre = x,
				 vline = vline,
				 extra_series = extra_series,
				 limits_y = limits_y)
	})
	names(all_plots) <- names(data_plots)
	if (y_as_plot) {
		x_lab = y_lab= NULL
		n_xlabel = 6 ;n_ylabel = 4;
		outDec = "."
		data_plot <- data.frame(date = as.numeric(time(y)),
								y = as.numeric(y))
		p <- ggplot2::ggplot(data = data_plot)
		if (vline) {
			for (date in out) {
				p <- p + ggplot2::geom_vline(xintercept = date, linetype = "dotted", alpha = 0.5)
			}
		}
		p <- p +
			ggplot2::geom_line(mapping = ggplot2::aes(x = date, y = y), linewidth = 0.7) +
			ggplot2::labs(title = "y", subtitle = NULL, x = x_lab,
						  y = y_lab) +
			ggplot2::scale_x_continuous(n.breaks = n_xlabel,
										labels = zoo::as.yearmon) +
			ggplot2::scale_y_continuous(n.breaks = n_ylabel,
										labels = function(x) format(x, decimal.mark = outDec)) +
			ggplot2::theme_bw() +
			ggplot2::theme(
				legend.position = "none",
				plot.title = ggplot2::element_text(hjust = 0.5),
				panel.grid.minor.x = ggplot2::element_blank(),
				panel.grid.major.x = ggplot2::element_blank(),
				axis.text.x = ggplot2::element_text(size=8, angle=20,
													vjust=1.1, hjust=1),
				axis.text.y = ggplot2::element_text(size=8),
				plot.subtitle = ggplot2::element_text(hjust = 0.5,
													  size=10,face="italic")
			)
		p
		all_plots <- c(list(y = p), all_plots)
	}
	all_plots
}

plot_y <- function(res, out = NULL, vline = TRUE,
				   titre = NULL, sous_titre = NULL,
				   nb_dates_before = 6, nb_after = 10,
				   outDec = ".",
				   n_xlabel = 5,
				   multiple_vline_out = FALSE) {
	x_lab = y_lab= NULL
    n_ylabel = 6;
	if (is.null(out)) {
		out <- res$out
	}
	if (length(out) > 1 & !multiple_vline_out) {
		all_plots <- lapply(out, plot_y, res = res, vline = vline,
							titre = titre, sous_titre = sous_titre,
							nb_dates_before = nb_dates_before, nb_after = nb_after,
							outDec = outDec)
		names(all_plots) <- out
		return(all_plots)
	}

	all_est <- create_vintage_est(res)
	y <- all_est$y[,ncol(all_est$y)]
	xlim <- c(start  = min(out) - nb_dates_before * deltat(y),
			  end  = max(out) + (nb_after - 1) * deltat(y)
	)
	ylim <- range(window(y, start = xlim[1], end = xlim[2]))
	dates <- time(ts(start = xlim[1], end = xlim[2], deltat = deltat(y)))
	x_breaks <- dates[seq.int(1, length((dates)), by = length((dates)) %/% n_xlabel)]

	data_plot <- data.frame(date = as.numeric(time(y)),
							y = as.numeric(y))

	p <- ggplot2::ggplot(data = data_plot)
	if (vline) {
		for (date in out) {
			p <- p + ggplot2::geom_vline(xintercept = date, linetype = "dotted", alpha = 0.5)
		}
	}
	p <- p +
		ggplot2::geom_line(mapping = ggplot2::aes(x = date, y = y), linewidth = 0.7) +
		ggplot2::labs(title = titre, subtitle = sous_titre, x = x_lab,
					  y = y_lab) +
		ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
		ggplot2::scale_x_continuous(breaks = x_breaks,
									labels = zoo::as.yearmon) +
		ggplot2::scale_y_continuous(n.breaks = n_ylabel,
									labels = function(x) format(x, decimal.mark = outDec)) +
		ggplot2::theme_bw() +
		ggplot2::theme(
			legend.position = "none",
			plot.title = ggplot2::element_text(hjust = 0.5),
			panel.grid.minor.x = ggplot2::element_blank(),
			panel.grid.major.x = ggplot2::element_blank(),
			axis.text.x = ggplot2::element_text(size=8),
			axis.text.y = ggplot2::element_text(size=8),
			plot.subtitle = ggplot2::element_text(hjust = 0.5,
												  size=10,face="italic")
		)
	p
}
