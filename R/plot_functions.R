# Plotting functions ---------------------------------------------------------------------

#' This function plots the two time series and correlation over time
#' There are optional arguments for confidence bands on x_var and customisation of the
#' maximum value of the y-axis and labels for the facets of grp_var
#' @param dat data frame with columns that correspond to two time series and grouping variable(s)
#' @param date_var character string of the column name that corresponds to the date variable
#' date_var must be specified if subset_date is not null.
#' @param grp_var character string of column names in dat to be used as grouping variable(s)
#' @param x_var primary time series (should be a column in dat)
#' @param y_var secondary time series (should be a column in dat)
#' @param x_var_lower column in dat corresponding to lower bound of confidence band for x_var
#' @param x_var_upper column in dat corresponding to upper bound of confidence band for x_var
#' @param legend_labels character vector of labels to use for plot legend
#' @param facet_labels vector of labels for facets (grp_var)
#' @param y_max maximum value of y-axis
#' @param col_values vector of color values
#' @return tibble of lags by grp_var
#' @keywords pika
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
    pivot_longer(-c(date, grp), names_to = "metric", values_to = "value") %>%
    filter(metric %in% c("x_var","y_var", "roll_corr"))

### Plot correlation, Rt, and movement by region
p <- ggplot(data = dat1, aes(x = date, y = x_var)) +
  geom_line(data = dat_long, aes(x = date, y = value, color = metric)) +
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



#' This function plots the lags by grp_var
#' @param dat data frame with columns that correspond to two time series and grouping variable(s)
#' @param lag_var character string of the column name that corresponds to the date variable
#' @param bins character string of column names in dat to be used as grouping variable(s)
#' @return tibble of lags by grp_var
#' @keywords pika
#' @export
plot_lag <- function(dat, lag_var, bins = 2){
# rename column names to work inside ggplot2 ---------------------------------------------
  dat1 <- dat %>%
    rename(lag = {{ lag_var }})

# make histogram of lags by grouping variable --------------------------------------------
p <- ggplot(dat, aes(x = lag)) +
  geom_histogram(binwidth = bins) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
        )

return(p)
}
