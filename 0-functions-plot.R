plot_est <- function (data, vline =NULL, titre = NULL, sous_titre = NULL, limits_y = NULL, outDec = ".") {
	x_lab = y_lab= NULL
	n_xlabel = 6 ;n_ylabel = 4;

	dataGraph <- format_data_plot(data)
	data_legend <- dataGraph |>
		dplyr::group_by(variable) |>
		dplyr::filter(date == max(date)) |>
		data.frame()
	p <- ggplot2::ggplot(data = dataGraph)
	if (!is.null(vline))
		p <- p + ggplot2::geom_vline(xintercept = vline, linetype = "dotted")

	p <- p +
		ggplot2::geom_line(mapping = aes(x = date, y = value, group = variable,
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

all_est <- create_vintage_est(res)
all_est2 <- create_vintage_est(res2)

plot_est(window(all_est$`LC robust`[,6:18], start = 2010))
library(gggplot2)
library(patchwork)
plot_est(window(all_est$LC[,6:18], start = 2010))
plot_est(window(all_est2$`LC robust`[,6:18], start = 2010))

