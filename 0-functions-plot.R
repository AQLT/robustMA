plot_est <- function (data, vline =NULL, titre = NULL, sous_titre = NULL, limits_y = NULL, outDec = ".",
					  extra_series = NULL, colour.extra_series = "darkgreen") {
	x_lab = y_lab= NULL
	n_xlabel = 6 ;n_ylabel = 4;

	dataGraph <- format_data_plot(data)
	data_legend <- dataGraph |>
		dplyr::group_by(variable) |>
		dplyr::filter(date == max(date)) |>
		data.frame()
	p <- ggplot2::ggplot(data = dataGraph)
	if (!is.null(vline))
		p <- p + ggplot2::geom_vline(xintercept = vline, linetype = "dotted", alpha = 0.5)
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
		ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = n_xlabel),
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

plot_confint <- function(data, out = NULL, default_filter, robust_f, nb_dates_before = 10, nest = 6, add_y = FALSE) {
	if (is.null(out)) {
		out <- data$out
	}
	if (!is.null(data$data))
		data <- data$data
	first_date <- min(out) - nb_dates_before * deltat(data[[1]])
	lagest <- seq(0, by = 1, length.out = nest)
	all_plots <- lapply(lagest, function(i){
		y <- data[[which(round(out,3) == round(as.numeric(names(data)),3)) + i]][,"y"]
		confint_robust <- confint_filter(y, robust_f[[sprintf("t%i", -i)]])
		confint_default <- confint_filter(y, lc_f)
		data_plot <- ts.union(confint_default, confint_robust[,1],y)
		data_plot <- window(data_plot, start = first_date)
		colnames(data_plot) <- c("Default", "Confint_m", "Confint_p", "Robust","y")
		data_plot <- data.frame(date = as.numeric(time(data_plot)), data_plot)
		p <- ggplot2::ggplot(data =data_plot, ggplot2::aes(x = date)) +
			ggplot2::geom_ribbon(ggplot2::aes(ymin = Confint_m, ymax = Confint_p),
								 fill = "grey") +
			ggplot2::geom_vline(xintercept = out,linetype= 2, alpha = 0.5) +
			ggplot2::geom_line(ggplot2::aes(y = Default), col = "blue")
		if (add_y)
			p <- p +
			ggplot2::geom_line(ggplot2::aes(y = y), col = "darkgreen")
		p <- p +
			ggplot2::geom_line(ggplot2::aes(y = Robust), col = "red") +
			ggplot2::labs(x = NULL, y = NULL, title = as.character(zoo::as.yearmon(out + i*deltat(y)))) +
			ggplot2::theme_bw() +
			ggplot2::scale_x_continuous(n.breaks = 6,
										labels = zoo::as.yearmon) +
			ggplot2::theme(
				legend.position = "none",
				plot.title = ggplot2::element_text(hjust = 0.5),
				panel.grid.minor.x = ggplot2::element_blank(),
				panel.grid.major.x = ggplot2::element_blank(),
				axis.text.x = ggplot2::element_text(size=8, angle=20,
													vjust=1.1, hjust=1),
				axis.text.y = ggplot2::element_text(size=8)
			)
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
		y_as_plot = FALSE){
	if (is.null(out)) {
		out <- res$out
	}
	if (length(out) > 1) {
		all_plots <- lapply(out, get_all_plots, res = res, nb_est = nb_est,
							nb_dates_before = nb_dates_before, vline = vline, add_y = add_y,
							y_as_plot = y_as_plot)
		names(all_plots) <- out
		return(all_plots)
	}
	all_est <- create_vintage_est(res)
	y <- all_est$y[,ncol(all_est$y)]
	all_est$y <- NULL
	cols <- seq.int(
		which(!is.na(window(all_est[[1]], start = out, end = out))),
		length.out = nb_est)
	data_plots <- lapply(all_est, `[`,, cols)
	data_plots <- lapply(data_plots, window,
						 start  = out - nb_dates_before * deltat(y),
						 end  = out + (nb_est - 1) * deltat(y))
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
	all_plots <- lapply(names(data_plots), function(x){
		plot_est(data_plots[[x]], titre = x,
				 vline = vline,
				 extra_series = extra_series)
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
				   nb_dates_before = 6, nb_after = 4,
				   outDec = ".") {
	x_lab = y_lab= NULL
	n_xlabel = 6 ;n_ylabel = 4;
	if (is.null(out)) {
		out <- res$out
	}
	all_est <- create_vintage_est(res)
	y <- all_est$y[,ncol(all_est$y)]
	y <- window(y,
				start  = min(out) - nb_dates_before * deltat(y),
				end  = max(out) + (nb_after - 1) * deltat(y)
	)
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
}
