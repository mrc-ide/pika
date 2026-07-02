# Plotting functions ---------------------------------------------------------------------

#' Plot time series and rolling correlation over time by group
#'
#' Produces a faceted line plot showing the primary series (\code{x_var}),
#' secondary series (\code{y_var}), and rolling correlation (\code{roll_corr})
#' over time, with one facet per group. Horizontal reference lines are drawn
#' at y = -1, 0, and 1. Optionally adds a shaded confidence ribbon around
#' \code{x_var}. The data frame must contain a column named \code{roll_corr}
#' (e.g. from \code{\link{rolling_corr}}).
#'
#' @param dat A data frame containing the two time series, a \code{roll_corr}
#'   column, a date column, and a grouping column.
#' @param date_var Character string giving the name of the date column (class
#'   \code{Date}).
#' @param grp_var Character string giving the name of the grouping column used
#'   for faceting.
#' @param x_var Character string giving the name of the primary time series column.
#' @param y_var Character string giving the name of the secondary time series column.
#' @param x_var_lower Character string giving the name of the column containing
#'   the lower confidence bound for \code{x_var}. If \code{NULL} (default), no
#'   ribbon is drawn. Both \code{x_var_lower} and \code{x_var_upper} must be
#'   supplied to draw a ribbon.
#' @param x_var_upper Character string giving the name of the column containing
#'   the upper confidence bound for \code{x_var}. If \code{NULL} (default), no
#'   ribbon is drawn.
#' @param facet_labels Named character vector of display labels for the facets,
#'   passed to \code{\link[ggplot2]{as_labeller}}. Names must match values in
#'   the grouping column. If \code{NULL} (default), raw group values are shown.
#' @param legend_labels Character vector of length 3 giving legend labels for
#'   \code{roll_corr}, \code{x_var}, and \code{y_var} respectively. If
#'   \code{NULL} (default), column names are used.
#' @param y_max Numeric. Maximum value for the y-axis. If supplied, the axis is
#'   set to \code{[-1, y_max]} and confidence bounds are clamped to this value.
#'   Default is \code{NULL} (auto-scaled).
#' @param col_values Character vector of length 3 specifying line colours for
#'   \code{roll_corr}, \code{x_var}, and \code{y_var} respectively. Defaults to
#'   dark purple, mid-green, and dark blue from RColorBrewer palettes.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @seealso \code{\link{rolling_corr}} to compute \code{roll_corr};
#'   \code{\link{plot_lag}} to visualise the lag distribution.
#'
#' @examples
#' \dontrun{
#' plot_corr(
#'   dat           = data_corr,
#'   date_var      = "date_end",
#'   grp_var       = "province",
#'   x_var         = "r_mean",
#'   y_var         = "movement",
#'   x_var_lower   = "r_q2.5",
#'   x_var_upper   = "r_q97.5",
#'   legend_labels = c("Rolling correlation", "Rt", "Mobility")
#' )
#' }
#'
#' @keywords pika
#' @import RColorBrewer
#' @import ggplot2
#' @import scales
#' @export
plot_corr <- function(dat, date_var, grp_var, x_var, y_var, x_var_lower = NULL,
                      x_var_upper = NULL, facet_labels = NULL, legend_labels = NULL,
                      y_max = NULL, col_values = c(brewer.pal(8, "RdPu")[8],
                                                   brewer.pal(8, "Greens")[5],
                                                   brewer.pal(8, "Blues")[8])){

# data wrangling for ggplot ------------------------------------------------------------
dat1 <- dat %>%
  rename(date = {{date_var}}, x_var = {{ x_var }}, y_var = {{ y_var }},grp = {{ grp_var }})

if(!is.null(x_var_lower) && !is.null(x_var_upper)){
  dat1 <- dat %>%
    rename(date = {{date_var}}, x_var = {{ x_var }}, y_var = {{ y_var }},
            x_var_lower = {{ x_var_lower }}, x_var_upper = {{ x_var_upper }},
            grp = {{ grp_var }})  # rename column names to work inside ggplot2 ----------
  if (!is.null(y_max)){
    dat1 <- dat1 %>%
      mutate(x_var_lower = ifelse(x_var_lower > y_max, y_max, x_var_lower),
             x_var_upper = ifelse(x_var_upper > y_max, y_max, x_var_upper))
  }
}


  # convert data to long format ----------------------------------------------------------
  dat_long <- dat1 %>%
    pivot_longer(-c("date", "grp"), names_to = "metric", values_to = "value") %>%
    filter(.data$metric %in% c("x_var","y_var", "roll_corr"))

### Plot correlation, Rt, and movement by region
p <- ggplot(data = dat1, aes(x = date, y = x_var)) +
  geom_line(data = dat_long, aes(x = date, y = .data$value, color = .data$metric)) +
  xlab("Date") + ylab("") +
  scale_x_date(labels = date_format("%Y-%m-%d")) +
  geom_hline(yintercept = 1, linetype="dashed", colour = "black") +
  geom_hline(yintercept = -1, linetype="dashed", colour = "black") +
  geom_hline(yintercept = 0, colour = "black") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
  )
# facets ---------------------------------------------------------------------------------
  if (is.null(facet_labels)){
    p <- p + facet_wrap( ~ grp)
  } else {
    p <- p + facet_wrap( ~ grp, labeller = as_labeller(facet_labels))
  }
# set maximum value for y-axis -----------------------------------------------------------
  if (!is.null(y_max)){
    p <- p + scale_y_continuous(limits = c(-1, y_max))
  }
# add confidence bounds ------------------------------------------------------------------
  if (!is.null(x_var_lower) && !is.null(x_var_upper)){
    p <- p + geom_ribbon(aes(ymin = x_var_lower, ymax = x_var_upper),
                         fill = col_values[2], alpha = 0.1)
  }
# customise legend -----------------------------------------------------------------------
  if (!is.null(col_values) && !is.null(legend_labels)){
    p <- p + scale_color_manual(values = col_values, name = "",
                                breaks = c("roll_corr", "x_var", "y_var"),
                                labels = legend_labels)
  }
  return(p)
}



#' Plot a histogram of optimal lags across groups
#'
#' Produces a histogram of the lag values returned by \code{\link{cross_corr}},
#' showing the distribution of optimal lags across groups.
#'
#' @param dat A data frame containing a lag column, typically the output of
#'   \code{\link{cross_corr}}.
#' @param lag_var Character string giving the name of the lag column to plot.
#' @param bins Numeric. Bin width for the histogram. Default is 2.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @seealso \code{\link{cross_corr}} to compute the lag values;
#'   \code{\link{plot_corr}} to visualise the time series and rolling correlation.
#'
#' @examples
#' \dontrun{
#' lags <- cross_corr(
#'   dat     = my_data,
#'   grp_var = "region",
#'   x_var   = "r_mean",
#'   y_var   = "movement"
#' )
#' plot_lag(lags, lag_var = "lag")
#' }
#'
#' @keywords pika
#' @export
plot_lag <- function(dat, lag_var, bins = 2){
# rename column names to work inside ggplot2 ---------------------------------------------
  dat1 <- dat %>%
    rename(lag = {{ lag_var }})

# make histogram of lags by grouping variable --------------------------------------------
p <- ggplot(dat1, aes(x = lag)) +
  geom_histogram(binwidth = bins) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
        )

return(p)
}
