rm(list = ls())
source("src/climatrend.R")

data_df <- read.csv(file.path(getwd(), "data", "PISCOt_Tmean_value.csv"), header = TRUE)[, -1]
climate_trend_out <- climatrend(t = data_df$years, y = data_df$values, width = 30)

data_df$CCM_trend <- climate_trend_out$trend
data_df$CCM <- mean(data_df[data_df$years > 1991 & data_df$years < 2020, ]$values)
data_df <- transform(data_df,
                     anom_CCM_trend = values - CCM_trend,
                     col_CCM_trend = ifelse(values > CCM_trend, "1", ifelse(values < CCM_trend, "2", "0")),
                     anom_CCM = values - CCM,
                     col_CCM = ifelse(values > CCM, "1", ifelse(values < CCM, "2", "0")))

ggplot2::theme_set(ggplot2::theme_minimal())

ggplot2::ggplot() +
  ggplot2::geom_point(data = data_df, ggplot2::aes(x = years, y = values), size = 2, colour = "gray70") +
  ggplot2::geom_line(data = data_df, ggplot2::aes(x = years, y = values), colour = "gray50", alpha = .7) + 
  ggplot2::geom_line(data = data_df, ggplot2::aes(x = years, y = CCM_trend), colour = "#009E73", linewidth = 1.5)  + 
  ggplot2::ylab("(mean) Air temperature (°C) for Peru\n Source: PISCOt v1.2") + ggplot2::xlab("") + ggplot2::ggtitle("Changing Current Climate Mean as Scherrer et al. (2024)") + 
  ggplot2::annotate(geom = "text", x = 1993, y = 23.75, label = "Climate Mean: local linear regression", color = "#009E73", alpha = .7) +
  ggplot2::scale_y_continuous(limits = c(21, 24)) -> p1

ggplot2::ggplot() +
  ggplot2::geom_point(data = data_df, ggplot2::aes(x = years, y = values), size = 2, colour = "gray70") +
  ggplot2::geom_line(data = data_df, ggplot2::aes(x = years, y = values), colour = "gray50", alpha = .7) + 
  ggplot2::geom_hline(yintercept =  unique(data_df$CCM), colour = "#009E73", linewidth = 1.5)   + 
  ggplot2::ylab("(mean) Air temperature (°C) for Peru\n Source: PISCOt v1.2") + ggplot2::xlab("") + ggplot2::ggtitle("No Changing Current Climate Mean as WMO") +
  ggplot2::annotate(geom = "text", x = 1994, y = 23.75, label = "Climate Mean: simple mean (1991-2020)", color = "#009E73", alpha = .7) +
  ggplot2::scale_y_continuous(limits = c(21, 24)) -> p2

ggplot2::ggplot() +
  ggplot2::geom_bar(data = data_df, ggplot2::aes(x = years, y = anom_CCM, fill = col_CCM), stat = "identity") +
  ggplot2::theme(legend.position = "none") + ggplot2::xlab("") + ggplot2::ylab("") +
  ggplot2::scale_fill_manual(values = c("#D55E00", "#56B4E9")) +
  ggplot2::scale_y_continuous(limits = c(-.8, .8)) + ggplot2::ggtitle("Deviation with No Changing Current Climate Mean") -> p3


ggplot2::ggplot() +
  ggplot2::geom_bar(data = data_df, ggplot2::aes(x = years, y = anom_CCM_trend, fill = col_CCM_trend), stat = "identity")  +
  ggplot2::scale_fill_manual(values = c("#D55E00", "#56B4E9")) +
  ggplot2::theme(legend.position = "none") + ggplot2::xlab("") + ggplot2::ylab("") +
  ggplot2::scale_y_continuous(limits = c(-.8, .8))  + ggplot2::ggtitle("Deviation with Changing Current Climate Mean") -> p4

cowplot::plot_grid(p2, p3, p1, p4, ncol = 2)

