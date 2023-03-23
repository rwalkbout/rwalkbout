library(tidyverse)
library(lubridate)
library(knitr)
library(ggforce)
library(lwgeom)
library(sf)
library(geosphere)
library(measurements)
library(patchwork)



# Plotting function
bout_plot <- function(id, gps_thresh, low_active_thresh, epoch_series_df, leading_minutes = 8, trailing_minutes = 12, gps_target_size = 0.25){
  ## CIRCLE PLOT ##
  # circle plot setup
  Point_ind <- epoch_series_df %>%
    filter((gps_inlier == TRUE | incomplete_GPS == 1) & !is.na(bout_label)) %>%
    select(c("LONGITUDE", "LATITUDE", "Point_radius", "incomplete_GPS")) %>%
    mutate(Point_radius = ifelse(is.na(Point_radius), 0, Point_radius))
  row_count <- nrow(Point_ind %>% filter(!is.na(LATITUDE)))
  P_radii <- max(Point_ind$Point_radius)
  incomplete_GPS <- max(Point_ind$incomplete_GPS)

  if(gps_thresh > P_radii){
    plot_dat <- data.frame(x0 = 1:(gps_thresh*2.2), y0 = 1:(gps_thresh*2.2)) %>% mutate(alpha = 0.2)} else{
      plot_dat <- data.frame(x0 = 1:(P_radii*2.2), y0 = 1:(P_radii*2.2)) %>% mutate(alpha = 0.2)
    }
  colors <- list(threshold = "skyblue4", data_radius = "palegreen4")
  # circle plotting code
  p <- ggplot() +
    coord_fixed() +
    theme_void()
  thresh_circle <- geom_circle(aes(x0=0, y0=0, r = gps_thresh),
                               color = colors$threshold, fill = colors$threshold, show.legend = TRUE, data = plot_dat)
  bout_circle <-  geom_circle(aes(x0=0, y0=0, r = P_radii),
                              color = colors$data_radius, fill = colors$data_radius, show.legend = TRUE, data = plot_dat)
  if(any(is.na(epoch_series_df$incomplete_GPS))){
    if(gps_thresh>P_radii) {
      circles <- p +
        thresh_circle +
        bout_circle
      title <- paste0("Dwell Bout (radius = ", P_radii %>% round(2), " feet)")
      title_color <- colors$threshold } else{
        circles <- p +
          bout_circle +
          thresh_circle
        title <- paste0("Non-Dwell Bout (radius = ", P_radii %>% round(2), " feet)")
        title_color <- colors$data_radius
      }
  } else{
    circles <- p + thresh_circle
    title <- paste0("Incomplete GPS Coverage")
    title_color <- colors$threshold
  }
  ## ACCELEROMETRY PLOT
  start <- with_tz(min(phil_start_time, w_start_time)) - minutes(leading_minutes)
  end <- with_tz(max(phil_end_time, w_end_time)) + minutes(trailing_minutes)
  df <- epoch_series_df %>%
    mutate(active = ifelse(Axis1_epochSum > low_active_thresh, "high", "low")) %>%
    mutate(epoch_time = ymd_hms(epoch_time)) %>%
    filter(epoch_time > start) %>%
    filter(epoch_time < end)
  xmax <- end
  xmin <- start + (1 - gps_target_size)*(end - start)
  y_low <- 0
  y_high <- max(df$Axis1_epochSum)*1.2
  ymax <- y_high
  ymin <- (1 - gps_target_size) * y_high
  plot <- ggplot(df, aes(x = epoch_time, y = Axis1_epochSum)) +
    geom_point() +
    geom_hline(yintercept=low_active_thresh, linetype="dashed", color = "gray") +
    xlim(as.POSIXct(start), as.POSIXct(end)) +
    ylim(y_low, y_high) +
    geom_text(aes(end, low_active_thresh, label = "Active", size = 12)) +
    geom_line() +
    ggtitle(paste(title)) +
    labs(x = "Time",
         y = "Accelerometer Counts",
         subtitle = paste0(row_count, " GPS data points"),
         caption = "Pink shaded area = Phil's Bout, Gray = Weipeng's bout") +
    theme_bw() +
    theme(legend.position = "none",
          panel.border = element_blank(),
          plot.caption = element_text(color = "darksalmon", size = 15, hjust = 0),
          plot.title = element_text(color = title_color, size = 20)) +
    annotate('rect', xmin=phil_start_time, xmax=phil_end_time, ymin=0, ymax=max(df$Axis1_epochSum), alpha=.3, fill='darksalmon') +
    annotate('rect', xmin=w_start_time, xmax=w_end_time, ymin=0, ymax=max(df$Axis1_epochSum), alpha=.3) +
    annotation_custom(ggplotGrob(circles), xmin = xmin, xmax = end, ymin = ymin, ymax = ymax)
  return(plot)
}
