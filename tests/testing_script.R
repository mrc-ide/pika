# Testing Script ------------------------------------------------------------------------

# Load required packages ----------------------------------------------------------------
# library(dplyr)
# library(tidyr)
# library(TTR)
# library(EpiEstim)
# library(ggplot2)
# library(scales)
# library(RColorBrewer)

# Load data -----------------------------------------------------------------------------
load("data/china_rt_estimates.rda")
load("data/exante_movement_data.rda")

# Join data sets together by date and province to determine cross correlation -----------
data_joined <- left_join(china_rt_estimates,
                         exante_movement_data,
                         by = c("date","province")
                         )

# Determine lag with max cross correlation between Rt and movement ----------------------
lags <- cross_corr(dat = data_joined,
                   date_var = "date",
                   grp_var = "province",
                   x_var = "r_mean",
                   y_var = "movement",
                   max_lag = 10,
                   subset_date = '2020-02-15'
                  )

# use min lag across groups -------------------------------------------------------------
my_lag <- min(lags$lag)

# create lag date using max lag from cross_corr() ---------------------------------------
data_joined_lag <- china_rt_estimates %>%
  mutate(date = date + my_lag) %>%
  left_join(., exante_movement_data, by = c("date", "province"))

# Determine rolling correlation between Rt and movement ---------------------------------
data_corr <- rolling_corr(dat = data_joined_lag,
                          grp_var = "province",
                          x_var = "r_mean",
                          y_var = "movement",
                          n = 14)

# Plot Rt, movement, and correlation ----------------------------------------------------
my_labels <- c("beijing" = "Beijing", "guandong" = "Guangdong", "henan" = "Henan",
               "hong_kong_sar" = "Hong Kong SAR", "hubei" = "Hubei", "hunan" = "Hunan",
               "zhejiang" = "Zhejiang")
my_legend = c("Correlation", "Reproduction Number", "Movement")

plot_corr(dat = data_corr,
          date_var = "date",
          grp_var = "province",
          x_var = "r_mean",
          y_var = "movement",
          x_var_lower = "r_q2.5",
          x_var_upper = "r_q97.5",
          facet_labels = my_labels,
          y_max = 10
          )
