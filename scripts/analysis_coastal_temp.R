# analysis_coastal_temp.R
# Beginner-friendly single R script for coastal ocean temperature by depth
# - Uses tidyverse for data manipulation and ggplot2 for plotting
# - Filters to years 2019-2023 (user requested)
# - Saves simple PNGs to outputs/figures

# Load libraries
library(tidyverse)   # includes ggplot2, dplyr, tidyr, readr
library(lubridate)   # for date handling

# Create output folder if missing (handle different working directories: project root or report/)
cands <- c(
  file.path(getwd(), "outputs", "figures"),
  file.path(getwd(), "..", "outputs", "figures"),
  file.path(getwd(), "..", "..", "outputs", "figures")
)
# choose existing candidate or default to first
found <- cands[dir.exists(cands)]
if (length(found) > 0) {
  out_fig <- found[1]
} else {
  out_fig <- cands[1]
}
if (!dir.exists(out_fig)) dir.create(out_fig, recursive = TRUE)
# Ensure out_fig is available in the global environment (helps when the script is sourced from other working directories)
assign("out_fig", out_fig, envir = .GlobalEnv)
message('Using out_fig = ', out_fig, ' (created if missing)')

# URLs from the TidyTuesday dataset description
url_temp <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-31/ocean_temperature.csv'
# (deployments file is available if needed)
# url_depl <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-31/ocean_temperature_deployments.csv'

# Read data (simple, no extra options)
message('Reading data from ', url_temp)
ocean_temperature <- readr::read_csv(url_temp, show_col_types = FALSE)

# Basic cleaning: keep rows with temperature, parse date, extract depth and year
df <- ocean_temperature %>%
  filter(!is.na(mean_temperature_degree_c)) %>%
  mutate(
    date = as.Date(date),                     # convert to Date
    depth_m = sensor_depth_at_low_tide_m,     # depth in meters (from dataset)
    year = year(date)                         # extract year
  )

# Remove point-level outliers before aggregation:
# - Round depth to nearest meter and compute 1st and 99th percentiles per depth
# - Keep only temperature values within these bounds to avoid plotting sensor errors
# This is simple and conservative for beginners; adjust percentiles if desired.
df <- df %>% mutate(depth_round = round(depth_m))
bounds <- df %>%
  group_by(depth_round) %>%
  summarise(low = quantile(mean_temperature_degree_c, probs = 0.01, na.rm = TRUE),
            high = quantile(mean_temperature_degree_c, probs = 0.99, na.rm = TRUE),
            .groups = 'drop')

# Join bounds and filter
before_n <- nrow(df)
df <- df %>% left_join(bounds, by = 'depth_round') %>%
  filter(mean_temperature_degree_c >= low, mean_temperature_degree_c <= high) %>%
  select(-low, -high)
after_n <- nrow(df)
message('Removed ', before_n - after_n, ' outlier rows (using 1%-99% per rounded depth).')

# Check which years are present and ensure 2018-2024 are available
wanted_years <- 2018:2024
present_years <- sort(unique(df$year))
missing_years <- setdiff(wanted_years, present_years)
if (length(missing_years) > 0) {
  message('Warning: the following requested years are missing from the data: ', paste(missing_years, collapse = ', '))
} else {
  message('All requested years (2019-2023) are present')
}

# Filter to requested years (if some are missing, this keeps available ones)
df <- df %>% filter(year %in% wanted_years)

# 1) Heatmap: monthly mean temperature by depth
# - Aggregate by month and rounded depth to keep plot simple
p_heat_data <- df %>%
  mutate(month = floor_date(date, 'month'),
         depth_round = round(depth_m)) %>%
  group_by(month, depth_round) %>%
  summarise(mean_temp = mean(mean_temperature_degree_c, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(mean_temp))

p1 <- ggplot2::ggplot(p_heat_data, aes(x = month, y = depth_round, fill = mean_temp)) +
  geom_tile() +
  scale_y_reverse(expand = c(0,0)) +  # depth increases downward
  scale_fill_viridis_c(name = 'Temperature (°C)') +
  scale_x_date(date_labels = '%Y-%m', date_breaks = '3 months') +
  labs(title = 'Coastal ocean temperature: monthly × depth', x = 'Month', y = 'Depth (m)') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot2::ggsave(filename = file.path(out_fig, 'heatmap_temp_depth_time.png'), plot = p1, width = 10, height = 4, dpi = 300)

# 2) Depth profiles: one line per year (2019-2023)
# - Compute mean temperature by year and depth
profiles <- df %>%
  group_by(year, depth_m) %>%
  summarise(mean_temp = mean(mean_temperature_degree_c, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(mean_temp))

p2 <- ggplot2::ggplot(profiles, aes(x = mean_temp, y = depth_m, color = factor(year), group = factor(year))) +
  geom_line(linewidth = 1) +
  scale_y_reverse() +
  labs(title = paste0('Depth profiles: ', min(wanted_years), '-', max(wanted_years)), x = 'Mean temperature (°C)', y = 'Depth (m)', color = 'Year') +
  theme_minimal()

ggplot2::ggsave(filename = file.path(out_fig, 'depth_profiles.png'), plot = p2, width = 6, height = 5, dpi = 300)

# 3) Yearly temperature by depth (binned depths for clarity)
# - Use a simple bin size (5 m) to reduce number of lines
bin_size <- 5
yearly_binned <- df %>%
  mutate(depth_bin = round(depth_m / bin_size) * bin_size) %>%
  group_by(year, depth_bin) %>%
  summarise(mean_temp = mean(mean_temperature_degree_c, na.rm = TRUE), .groups = 'drop') %>%
  tidyr::complete(depth_bin, year = wanted_years) %>%
  arrange(depth_bin, year)

# Expose variable when sourced interactively
assign('yearly_binned', yearly_binned, envir = .GlobalEnv)

# Note: keep NA mean_temp so geom_line breaks at missing years (prevents connecting across gaps)
# Only plot/save if there is at least one non-missing mean_temp value
if (nrow(dplyr::filter(yearly_binned, !is.na(mean_temp))) > 0) {
  p3 <- ggplot2::ggplot(yearly_binned, aes(x = year, y = mean_temp, color = factor(depth_bin), group = factor(depth_bin))) +
    geom_line(linewidth = 0.9) +
    labs(title = 'Yearly mean temperature by depth (binned)', x = 'Year', y = 'Mean temperature (°C)', color = 'Depth (m)') +
    theme_minimal()

  ggplot2::ggsave(filename = file.path(out_fig, 'yearly_temp_by_depth.png'), plot = p3, width = 8, height = 5, dpi = 300)
} else {
  message('No data available to create yearly_temp_by_depth plot (all mean_temp are NA)')
}


# Simple summary: mean temperature by year (console output)
summary_by_year <- df %>%
  group_by(year) %>%
  summarise(mean_temp = mean(mean_temperature_degree_c, na.rm = TRUE), n = n(), .groups = 'drop')
message('Summary (mean temp by year):')
print(summary_by_year)

message('Plots saved to ', out_fig)
