library(tidyverse)
library(epiextractr)

# ── Labels ────────────────────────────────────────────────────────────────────

vars_of_interest <- c("ur_total16", "ur_black", "ur_asian", "ur_teens", "dou_27weeks")
horizons         <- c("m1", "m3", "m6", "m12")

var_labels <- c(
  ur_total16  = "Unemployment Rate, 16+",
  ur_black    = "Unemployment Rate, Black Workers",
  ur_asian    = "Unemployment Rate, Asian Workers",
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
  ur_asian    = "Percentage Points",
  ur_teens    = "Percentage Points",
  dou_27weeks = "Thousands of People"
)

# Which CPS basic demographic group corresponds to each variable
sample_group <- c(
  ur_total16  = "total",
  ur_black    = "black",
  ur_asian    = "asian",
  ur_teens    = "teens",
  dou_27weeks = "total"
)

sample_labels <- c(
  total = "All persons 16+",
  black = "Black persons 16+",
  asian = "Asian persons 16+",
  teens = "Persons age 16–19"
)

pandemic_start <- as.Date("2020-03-01")
pandemic_end   <- as.Date("2021-04-01")

# ── 1. Change data from processed PDFs ───────────────────────────────────────

df_long <- read_csv("inputs/processed_pdfs.csv", show_col_types = FALSE) %>%
  filter(var %in% vars_of_interest) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  pivot_longer(
    cols          = matches("(actual|needed)_m"),
    names_to      = c(".value", "horizon"),
    names_pattern = "(.+)_(m\\d+)"
  ) %>%
  filter(horizon %in% horizons)

make_change_chart <- function(var_name, horizon_code, exclude_pandemic = FALSE) {
  pd <- df_long %>% filter(var == var_name, horizon == horizon_code)

  if (exclude_pandemic)
    pd <- pd %>% filter(!(date >= pandemic_start & date <= pandemic_end))

  pd <- pd %>% complete(date = seq(min(date), max(date), by = "month"))

  sub <- if (exclude_pandemic)
    "Estimates outside the shaded band are significant at the 90% level  |  Mar 2020–Apr 2021 excluded"
  else
    "Estimates outside the shaded band are significant at the 90% level"

  ggplot(pd, aes(x = date, y = actual)) +
    geom_ribbon(aes(ymin = -needed, ymax = needed),
                fill = "steelblue", alpha = 0.3, na.rm = TRUE) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    labs(
      title    = paste0(var_labels[var_name], " — ", horizon_labels[horizon_code]),
      subtitle = sub,
      x = NULL, y = y_labels[var_name]
    ) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    theme_minimal() +
    theme(
      axis.text.x   = element_text(angle = 45, hjust = 1),
      plot.title    = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray50", size = 9)
    )
}

# ── 2. Sample sizes from CPS basic ───────────────────────────────────────────

message("Loading CPS basic monthly data for sample sizes...")
cps <- load_basic(2016:2026, year, month, age, wbhao)

sample_sizes <- cps %>%
  filter(age >= 16) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  group_by(date) %>%
  summarise(
    total = sum(age >= 16, na.rm = TRUE),
    black = sum(as.integer(wbhao) == 2L, na.rm = TRUE),
    asian = sum(as.integer(wbhao) == 4L, na.rm = TRUE),
    teens = sum(age >= 16 & age <= 19, na.rm = TRUE),
    .groups = "drop"
  )

make_sample_chart <- function(var_name, exclude_pandemic = FALSE) {
  grp <- sample_group[var_name]

  pd <- sample_sizes %>% select(date, obs = all_of(unname(grp)))

  if (exclude_pandemic)
    pd <- pd %>% filter(!(date >= pandemic_start & date <= pandemic_end))

  pd <- pd %>% complete(date = seq(min(date), max(date), by = "month"))

  sub <- paste0(
    "Unweighted CPS basic monthly observations: ", sample_labels[grp],
    if (exclude_pandemic) "  |  Mar 2020–Apr 2021 excluded" else ""
  )

  ggplot(pd, aes(x = date, y = obs)) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    labs(
      title    = paste0(var_labels[var_name], " — Sample Size"),
      subtitle = sub,
      x = NULL, y = "Number of Observations"
    ) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(
      axis.text.x   = element_text(angle = 45, hjust = 1),
      plot.title    = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray50", size = 9)
    )
}

# ── 3. Generate all charts ────────────────────────────────────────────────────

dir.create("output", showWarnings = FALSE)

for (v in vars_of_interest) {
  for (pandemic in c(FALSE, TRUE)) {
    suffix <- if (pandemic) "_nopandemic" else ""

    # Change charts (4 horizons each)
    for (h in horizons) {
      p <- make_change_chart(v, h, pandemic)
      ggsave(file.path("output", paste0(v, "_", h, suffix, ".png")),
             p, width = 10, height = 6, dpi = 150)
      message("Saved: ", v, "_", h, suffix, ".png")
    }

    # Sample size chart
    p <- make_sample_chart(v, pandemic)
    ggsave(file.path("output", paste0(v, "_sample", suffix, ".png")),
           p, width = 10, height = 6, dpi = 150)
    message("Saved: ", v, "_sample", suffix, ".png")
  }
}
