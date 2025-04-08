library(ggplot2)
library(dplyr)

generate_precipitation_plots <- function(data) {
  data <- data %>%
    filter(!is.na(time), !is.na(ptype), !is.na(park)) %>%
    filter(time >= as.Date("2005-01-01"), time <= as.Date("2025-12-31")) %>%
    filter(is.finite(ptype))
  
  if (nrow(data) == 0) stop("No data available after filtering.")
  
  parks <- unique(data$park)
  
  ref_lines <- data.frame(
    y = 1:4,
    label = c("Rain (1)", "Snow (2)", "Freezing Rain (3)", "Ice Pellets (4)")
  )
  
  plots_list <- list()
  
  for (p in parks) {
    park_data <- data %>% filter(park == p)
    
    plots_list[[p]] <- ggplot(park_data, aes(x = time, y = ptype)) +
      geom_line(color = "steelblue") +
      geom_hline(data = ref_lines, aes(yintercept = y), linetype = "dashed", color = "gray50") +
      geom_text(data = ref_lines, aes(x = min(park_data$time), y = y, label = label),
                hjust = 0, vjust = -0.5, size = 3, color = "gray30", inherit.aes = FALSE) +
      labs(
        title = paste("Precipitation Over Time -", p),
        x = "Time",
        y = "Precipitation Type"
      ) +
      theme_minimal()
  }
  
  plots_list[["All Parks Combined"]] <- ggplot(data, aes(x = time, y = ptype, color = park)) +
    geom_line() +
    geom_hline(data = ref_lines, aes(yintercept = y), linetype = "dashed", color = "gray50") +
    geom_text(data = ref_lines, aes(x = min(data$time), y = y, label = label),
              hjust = 0, vjust = -0.5, size = 3, color = "gray30", inherit.aes = FALSE) +
    labs(
      title = "Precipitation Over Time for All Parks",
      x = "Time",
      y = "Precipitation Type",
      color = "Park"
    ) +
    theme_minimal()
  
  return(plots_list)
}
