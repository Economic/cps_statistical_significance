library(tidyverse)

df <- read_csv("inputs/processed_pdfs.csv")

vars_of_interest <- c("ur_total16", "ur_black", "ur_teens", "dou_27weeks")
horizons         <- c("m1", "m3", "m6", "m12")

var_labels <- c(
  ur_total16  = "Unemployment Rate, 16+",
  ur_black    = "Unemployment Rate, Black Workers",
  ur_teens    = "Unemployment Rate, Teenagers",
  dou_27weeks = "Long-Term Unemployment (27+ Weeks)"
)

horizon_labels <- c(
  m1  = "1-Month Change",
  m3  = "3-Month Change",
  m6  = "6-Month Change",
  m12 = "12-Month Change"
)

y_labels <- c(
  ur_total16  = "Percentage Points",
  ur_black    = "Percentage Points",
  ur_teens    = "Percentage Points",
  dou_27weeks = "Thousands of People"
)

df_long <- df %>%
  filter(var %in% vars_of_interest) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  pivot_longer(
    cols          = matches("(actual|needed)_m"),
    names_to      = c(".value", "horizon"),
    names_pattern = "(.+)_(m\\d+)"
  ) %>%
  filter(horizon %in% horizons)

make_chart <- function(var_name, horizon_code) {
  plot_data <- df_long %>%
    filter(var == var_name, horizon == horizon_code) %>%
    complete(date = seq(min(date), max(date), by = "month"))

  ggplot(plot_data, aes(x = date, y = actual)) +
    geom_ribbon(
      aes(ymin = -needed, ymax = needed),
      fill = "steelblue", alpha = 0.3, na.rm = TRUE
    ) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    labs(
      title    = paste0(var_labels[var_name], " — ", horizon_labels[horizon_code]),
      subtitle = "Estimates outside the shaded band are significant at the 90% level",
      x        = NULL,
      y        = y_labels[var_name]
    ) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    theme_minimal() +
    theme(
      axis.text.x   = element_text(angle = 45, hjust = 1),
      plot.title    = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray50", size = 9)
    )
}

dir.create("output", showWarnings = FALSE)

for (v in vars_of_interest) {
  for (h in horizons) {
    p <- make_chart(v, h)
    ggsave(
      filename = file.path("output", paste0(v, "_", h, ".png")),
      plot     = p,
      width    = 10,
      height   = 6,
      dpi      = 150
    )
    message("Saved: ", v, "_", h, ".png")
  }
}
