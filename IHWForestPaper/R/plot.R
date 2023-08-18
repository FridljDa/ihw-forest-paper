#' Plot FDR and Power
#'
#' This function takes a data frame \code{sim_res} with simulation results and generates
#' two plots showing the False Discovery Rate (FDR) and Power for different grouping 
#' dimensions and methods.
#'
#' @param sim_res A data frame containing simulation results.
#' @param group_by_dimension The column name in \code{sim_res} to be used for grouping.
#' @param alpha The significance level used to plot the FDR threshold line.
#' @return A combined ggplot object showing FDR and Power plots with a common legend.
#' @import magrittr
#' @import dplyr
#' @importFrom ggpubr ggarrange
#' @import ggplot2
#'
#' @examples
#' sim_res <- data.frame(
#'   length = rep(c(10, 20, 30), each = 5),
#'   method = rep(c("A", "B"), each = 15),
#'   FDP = c(0.1, 0.2, 0.05, 0.15, 0.03, 0.08, 0.25, 0.18, 0.09, 0.12,
#'           0.2, 0.15, 0.07, 0.05, 0.1, 0.2, 0.25, 0.3, 0.35, 0.4),
#'   pow = c(0.9, 0.8, 0.85, 0.92, 0.88, 0.92, 0.81, 0.85, 0.9, 0.95,
#'           0.7, 0.75, 0.8, 0.82, 0.78, 0.72, 0.68, 0.65, 0.62, 0.6)
#' )
#' plot_fdr_power(sim_res, group_by_dimension = "length")
#'
#' @export
plot_fdr_power <- function(sim_res, group_by_dimension = "length", alpha = 0.1, log_trans = TRUE) {
  sim_res$group_by_dimension <- sim_res[, group_by_dimension]
  
  sim_res <- sim_res %>%
    group_by(group_by_dimension, method) %>%
    summarize(
      FDR = mean(FDP, na.rm = TRUE),
      Power = mean(pow, na.rm = TRUE),
      # Power_lower = quantile() #TODO
      n_monte_carlo = sum(!is.na(FDP)),
      pow_se = sd(pow, na.rm = TRUE) / sqrt(n_monte_carlo)
    ) %>%
    filter(!is.na(FDR)) %>%
    ungroup()
  
  if (length(unique(sim_res$n_monte_carlo)) > 1) {
    print("Warning: n_monte_carlo is not constant across groups.")
    print(table(sim_res$method, sim_res$n_monte_carlo))
  }
  
  #TODO if sim_res$n_monte_carlo is not constant, print a message 
  
  breaks <- unique(sim_res$group_by_dimension)
  if(length(breaks) < 5) {
    breaks <- breaks
  } else {
    breaks <- pretty(breaks, n = 5) # You can adjust 'n' as needed
  }
  
  # FDR
  sim_res_FDR <- ggplot(sim_res, aes(x = group_by_dimension, y = FDR, shape = method, col = method)) +
    geom_line() +
    geom_point(size = 2) +
    geom_hline(yintercept = alpha, linetype = 2) +
    guides(
      color = guide_legend(nrow=2, title = "Method", legend.text = element_text(size = 4)),
      shape = guide_legend(nrow=2, title = "Method", legend.text = element_text(size = 4)),
      nrow=2
    ) +
    xlab(group_by_dimension) +
    scale_x_continuous(breaks = breaks) +
    ylim(0, NA)
  
  # power
  sim_res_power <- ggplot(sim_res, aes(x = group_by_dimension, y = Power, shape = method, col = method)) +
    geom_line() +
    geom_point(size = 2) +
    ylab("Power") +
    xlab(group_by_dimension) +
    guides(
      color = guide_legend(title = "Method", legend.text = element_text(size = 4)),
      shape = guide_legend(title = "Method", legend.text = element_text(size = 4))
    ) +
    scale_x_continuous(breaks = breaks) +
    ylim(0, NA)
  
  if(log_trans){
    sim_res_FDR <- sim_res_FDR + scale_y_log10()
    sim_res_power <- sim_res_power + scale_y_log10()
  }
  g_combined <- ggpubr::ggarrange(sim_res_FDR, sim_res_power,
                          nrow = 1, widths = c(1, 1),
                          common.legend = TRUE, legend = "bottom"
  )
  
  return(g_combined)
}

#' Plot a 2D Simulation
#'
#' This function takes two-dimensional data (Xs) and a corresponding vector of categories (Hs) to create a scatter plot. An optional title can be added using a named list with multiple elements.
#'
#' @param Xs A matrix or data frame containing the 2D coordinates. It must have two columns.
#' @param Hs A vector containing the categories or labels for the points. It must have the same length as the number of rows in Xs.
#' @param title_list An optional named list containing the title information. If provided, the names and elements will be concatenated and used as the title for the plot.
#' @return A ggplot object representing the scatter plot.
#' @examples
#' Xs <- matrix(rnorm(100), ncol = 2)
#' Hs <- sample(letters[1:2], 50, replace = TRUE)
#' title_list <- list(main = "My 2D Simulation", subtitle = "An example plot")
#' plot_2d_simulation(Xs, Hs, title_list)
plot_2d_simulation <- function(Xs, Hs, title_list = NULL){
  if (ncol(Xs) != 2) stop("Xs must be a 2D matrix or data frame.")
  
  df <- data.frame(Xs1 = Xs[,1],
                   Xs2 = Xs[,2],
                   Hs = as.factor(Hs))
  
  plot <- ggplot(df, aes(x = Xs1, y = Xs2, color = Hs)) +
    geom_point()
  
  if (!is.null(title_list)) {
    title_parts <- mapply(function(name, value) paste(name, value, sep = ": "), names(title_list), title_list)
    title <- paste(title_parts, collapse = " - ")
    plot <- plot + ggtitle(title)
  }
  
  return(plot)
}
