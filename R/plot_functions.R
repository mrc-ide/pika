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
#' @param labels vector of labels for facets (grp_var)
#' @param y_max maximum value of y-axis
#' @param col_values vector of color values
#' @return tibble of lags by grp_var
#' @keywords pika
#' @export
plot_corr <- function(dat, date_var, grp_var, x_var, y_var, x_var_lower = NULL,
                      x_var_upper = NULL,labels = NULL, y_max = NULL, col_values = NULL){

  # data wrangling for ggplot ------------------------------------------------------------
  dat1 <- dat %>%
    rename(date = {{date_var}}, x_var = {{ x_var }}, y_var = {{ y_var }},
           grp = {{ grp_var }})  # rename column names to work inside ggplot2 ------------

  if(!is.null(x_var_lower) && !is.null(x_var_upper)){
    dat1 <- dat %>%
      rename(date = {{date_var}}, x_var = {{ x_var }}, y_var = {{ y_var }},
             x_var_lower = {{ x_var_lower }}, x_var_upper = {{ x_var_upper }},
             grp = {{ grp_var }})  # rename column names to work inside ggplot2 ----------
  }

  # convert data to long format ----------------------------------------------------------
  dat_long <- dat1 %>%
    pivot_longer(-c(date, grp), names_to = "metric", values_to = "value") %>%
    filter(metric %in% c("x_var","y_var", "roll_corr"))

### Plot correlation, Rt, and movement by region
ggplot(data = dat1, aes(x = date, y = x_var)) +
  geom_line(data = dat_long, aes(x = date_end, y = value, color = metric)) +
  geom_ribbon(aes(ymin=r_q2.5,ymax=r_q97.5), fill= "#00BA38" ,alpha=0.1)+
  xlab("Date") + ylab("") +
  scale_color_manual(values = c("#F8766D","#00BA38","#619CFF"), name = "",
                     breaks=c("trips_scaled", "r_mean", "roll_corr_weekly"),
                     labels = c("Number of Trips (in millions)", "Reproduction Number", "Correlation")) +
  scale_x_date(labels = date_format("%Y-%m-%d")) +
  scale_y_continuous(limits = c(-1, ymax)) +
  geom_hline(yintercept = 1, linetype="dashed", colour = "black") +
  geom_hline(yintercept = -1, linetype="dashed", colour = "black") +
  geom_hline(yintercept = 0, colour = "black") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap( ~ name, labeller = as_labeller(region_labels))
}



### plot London

london <- data_full3 %>% filter(region == "london") %>% mutate(trips_scaled = trips_scaled * 10)
london_long <- london %>%
  pivot_longer(-c(date_end, code, name, region, nation, journey_purpose), names_to = "metric",
               values_to = "value") %>%
  filter(metric %in% c("r_mean","trips_scaled", "roll_corr_weekly"), journey_purpose == "sum")
ymax = 18
london_sum <- london %>%
  filter(journey_purpose == "sum") %>%
  mutate(r_q2.5 = ifelse(r_q2.5 > ymax, ymax, r_q2.5),
         r_q97.5 = ifelse(r_q97.5 > ymax, ymax, r_q97.5))
### Plot correlation, Rt, and movement by region
p2 <- ggplot(data = london_sum, aes(x = date_end, y = r_mean)) +
  geom_line(data = london_long, aes(x = date_end, y = value, color = metric)) +
  geom_ribbon(aes(ymin=r_q2.5,ymax=r_q97.5), fill= "#00BA38" ,alpha=0.1)+
  xlab("Date") + ylab("") +
  scale_color_manual(values = c("#F8766D","#00BA38","#619CFF"), name = "",
                     breaks=c("trips_scaled", "r_mean", "roll_corr_weekly"),
                     labels = c("Number of Trips (x 100,000)", "Reproduction Number", "Correlation")) +
  scale_x_date(labels = date_format("%Y-%m-%d")) +
  scale_y_continuous(limits = c(-1, ymax)) +
  geom_hline(yintercept = 1, linetype="dashed", colour = "black") +
  geom_hline(yintercept = -1, linetype="dashed", colour = "black") +
  geom_hline(yintercept = 0, colour = "black") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap( ~ name)


p_lags <- ggplot(lag_df, aes(x=lag)) +
  geom_histogram(binwidth = 2) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )
p_lags_full <- ggplot(lag_df_full, aes(x=lag)) +
  geom_histogram(binwidth = 2) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )
