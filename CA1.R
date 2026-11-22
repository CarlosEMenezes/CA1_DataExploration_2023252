# =============================================================================
# CA1 - Data Exploration & Preparation
# Domain : Energy
# Dataset: Our World in Data (OWID) - Energy dataset (owid-energy-data.csv)
#
# Research question:
#   How has the electricity mix of the world's countries changed between
#   2000 and 2022, and which countries break the expected link between
#   wealth, energy use and clean electricity?
#
# Script layout (each section maps to one marking criteria):
#   0. Setup ............................................ (all)
#   1. Load, characterize and prepare the data .......... (Criteria 1)
#   2. Exploratory Data Analysis + unusual patterns ..... (Criteria 2)
#   3. Encoding schemes + PCA ........................... (Criteria 3)
#   4. Save outputs for the poster ...................... (Criteria 4)
#
# How to run: open CA1.Rproj in RStudio, then Source this file (Ctrl+Shift+S).
# All figures are written to ./figures and the clean dataset to ./data.
#
# Testing approach: after every preparation step there is a stopifnot()
# "data contract". If an assumption about the data is wrong, the script
# stops at that line instead of quietly producing misleading charts later.
# =============================================================================


# =============================================================================
# 0. SETUP
# =============================================================================

# Core data-wrangling and plotting toolkit (dplyr, tidyr, readr, ggplot2, purrr, stringr).
library(tidyverse)
# install.packages("devtools")
# devtools::install_github("r-lib/conflicted")
# Missing-value summaries and plots.
library(naniar)
# Maps ISO country codes to World Bank regions.
library(countrycode)
# Correlation heatmap.
library(corrplot)
# PCA visualisation helpers (scree plot, variable correlation circle).
library(factoextra)
# Pairwise scatter-plot matrix.
library(GGally)
# One-hot encoding.
library(fastDummies)
# Non-overlapping text labels on scatter plots (installed with factoextra).
library(ggrepel)


# Fixed seed so any random step (e.g. jitter) gives the same result on every run.
set.seed(2026)

# Output folders for figures and cleaned data. showWarnings = FALSE stays silent if they already exist.
dir.create("figures", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

# One consistent look for every chart, so the poster reads as one piece.
theme_set(theme_minimal(base_size = 12))

# Helper that saves a ggplot at poster quality (300 dpi).
# Wrapping ggsave() in a function keeps sizes and dpi identical across figures (DRY principle).
save_figure <- function(plot_object, file_name, width_in = 8, height_in = 5) {
  ggsave(filename = file.path("figures", file_name), plot = plot_object,
         width = width_in, height = height_in, dpi = 300, bg = "white")
}


# =============================================================================
# 1. LOAD, CHARACTERISE AND PREPARE THE DATA  (Criteria 1)
# =============================================================================

# ---- 1.1 Load the raw data ---------------------------------------------------

# read_csv() (readr) instead of base read.csv(): it is faster, never turns text
# into factors silently, and reports the column types it guessed.
raw_energy_data <- read_csv("owid-energy-data.csv", show_col_types = FALSE)

# Basic characterization: size and time span of the raw file.
cat("Raw dataset:", nrow(raw_energy_data), "rows x", ncol(raw_energy_data), "columns\n")
cat("Years covered:", min(raw_energy_data$year), "-", max(raw_energy_data$year), "\n")

# Data contract: the file I loaded is the full OWID energy file.
stopifnot(nrow(raw_energy_data) > 20000, ncol(raw_energy_data) > 100)


# ---- 1.2 Challenge 1: aggregate rows mixed in with countries -----------------

# OWID stores regions and income groups ("World", "Africa (EI)",
# "High-income countries") in the same table as countries. Those rows have no
# ISO country code. Leaving them in would double-count every country.
aggregate_rows <- raw_energy_data %>%
  filter(is.na(iso_code) | str_starts(iso_code, "OWID"))

cat("Aggregate (non-country) rows removed:", nrow(aggregate_rows), "\n")

# Show a few aggregate names as evidence for the poster.
print(head(unique(aggregate_rows$country), 15))

# Keep real countries only: a valid ISO code that is not an OWID-made code.
country_level_data <- raw_energy_data %>%
  filter(!is.na(iso_code), !str_starts(iso_code, "OWID"))

# Data contract: every remaining row has an ISO code.
stopifnot(!anyNA(country_level_data$iso_code))


# ---- 1.3 Challenge 2: data coverage changes over time ------------------------

# Count how many countries report each key variable in each year.
# This is the evidence for the 2000-2022 analysis window chosen below.
coverage_by_year <- country_level_data %>%
  filter(year >= 1980) %>%
  group_by(year) %>%
  summarise(across(c(gdp, energy_per_capita, coal_share_elec), ~ sum(!is.na(.x)))) %>%
  pivot_longer(-year, names_to = "variable", values_to = "countries_with_data")

coverage_plot <- ggplot(coverage_by_year, aes(year, countries_with_data, colour = variable)) +
  geom_line(linewidth = 1) +
  # Shade the analysis window so the choice is visible on the chart.
  annotate("rect", xmin = 2000, xmax = 2022, ymin = -Inf, ymax = Inf, alpha = 0.1) +
  labs(title = "Data coverage by year",
       subtitle = "Electricity-mix data jumps to ~200 countries in 2000; GDP stops after 2022",
       x = "Year", y = "Countries with data", colour = "Variable")
save_figure(coverage_plot, "01_coverage_by_year.png")


# ---- 1.4 Select the analysis window and the attributes -----------------------

# The eight generation sources. Their shares of electricity add up to 100 %.
electricity_share_columns <- c(
  "coal_share_elec", "gas_share_elec", "oil_share_elec", "nuclear_share_elec",
  "hydro_share_elec", "solar_share_elec", "wind_share_elec",
  "other_renewables_share_elec"
)

# Window 2000-2022: before 2000 the electricity-mix data covers only ~55 countries.
# GDP (Maddison Project) has no values after 2022.
selected_energy_data <- country_level_data %>%
  filter(between(year, 2000, 2022)) %>%
  select(country, iso_code, year, population, gdp,
         energy_per_capita, per_capita_electricity, carbon_intensity_elec,
         all_of(electricity_share_columns))

cat("After window + column selection:", nrow(selected_energy_data), "rows x",
    ncol(selected_energy_data), "columns,",
    n_distinct(selected_energy_data$country), "countries\n")


# ---- 1.5 Missing-value audit -------------------------------------------------

# Missing count and percentage per variable, sorted from most to least missing.
missing_value_summary <- miss_var_summary(selected_energy_data)
print(missing_value_summary)

# Visual version of the same audit, for the poster.
missing_value_plot <- gg_miss_var(selected_energy_data, show_pct = TRUE) +
  labs(title = "Missing values per attribute (2000-2022, countries only)")
save_figure(missing_value_plot, "02_missing_values_before.png", width_in = 7, height_in = 5)


# ---- 1.6 Challenge 3: GDP is missing for whole countries (not at random) -----

# Count the years of GDP each country has. Countries with zero are small
# islands and territories that the Maddison Project does not cover.
gdp_years_per_country <- selected_energy_data %>%
  group_by(country) %>%
  summarise(years_with_gdp = sum(!is.na(gdp)), .groups = "drop")

countries_without_gdp <- gdp_years_per_country %>% filter(years_with_gdp == 0)
cat("Countries with no GDP at all:", nrow(countries_without_gdp), "\n")
print(countries_without_gdp$country)

# These countries are dropped instead of imputed. With no GDP value at all,
# any imputation would be invented data. This missingness is not at random
# (it depends on country size), so the limitation is stated on the poster.
energy_data_with_gdp <- selected_energy_data %>%
  filter(!country %in% countries_without_gdp$country)


# ---- 1.7 Challenge 4: missing source shares are structural zeros -------------

# Hypothesis: when a source share is NA, the country simply has no plant of that
# type (e.g. no wind farms in Saudi Arabia), so the true value is 0.
# Test: on rows with a missing share, the shares that are present should
# already add up to 100 %. If they do, there is no room left for the missing one.
rows_with_missing_share <- energy_data_with_gdp %>%
  filter(if_any(all_of(electricity_share_columns), is.na))

# rowSums() with na.rm = TRUE adds only the shares that are present.
known_share_total <- rowSums(rows_with_missing_share[electricity_share_columns], na.rm = TRUE)
cat("Rows with at least one missing share:", nrow(rows_with_missing_share), "\n")
print(summary(known_share_total))

# Data contract: the hypothesis holds (allowing 0.5 points for rounding).
stopifnot(all(abs(known_share_total - 100) < 0.5))

# The hypothesis holds, so replacing NA with 0 restores the true value.
# Mean or median imputation here would be wrong: it would push the shares above 100 %.
energy_data_shares_fixed <- energy_data_with_gdp %>%
  mutate(across(all_of(electricity_share_columns), ~ replace_na(.x, 0)))


# ---- 1.8 Remaining gaps: interpolate or drop? --------------------------------

# Interpolation (e.g. zoo::na.approx) is only valid for gaps between two known
# years. This counts gaps strictly inside each country's observed range.
count_internal_gaps <- function(values) {
  observed_positions <- which(!is.na(values))
  if (length(observed_positions) == 0) return(0L)
  # Count NAs between the first and last observed value only.
  sum(is.na(values[min(observed_positions):max(observed_positions)]))
}

internal_gap_counts <- energy_data_shares_fixed %>%
  arrange(country, year) %>%
  group_by(country) %>%
  summarise(across(c(gdp, energy_per_capita, per_capita_electricity, carbon_intensity_elec),
                   count_internal_gaps), .groups = "drop")

cat("Internal gaps per variable (interpolation candidates):\n")
print(colSums(select(internal_gap_counts, -country)))

# The remaining NAs are leading gaps. Serbia and Montenegro report separately
# only from 2005/2006 (they were one state before), so filling these years
# would be extrapolation, not interpolation. Those few rows are dropped.
rows_to_drop <- energy_data_shares_fixed %>% filter(if_any(everything(), is.na))
cat("Rows dropped for leading gaps:", nrow(rows_to_drop), "\n")
print(count(rows_to_drop, country))

energy_data_complete <- energy_data_shares_fixed %>% drop_na()

# Data contract: no missing values remain.
stopifnot(!anyNA(energy_data_complete))


# ---- 1.9 Feature engineering -------------------------------------------------

# Data contract: values are strictly positive before taking logs (log(0) is -Inf).
stopifnot(all(energy_data_complete$energy_per_capita > 0),
          all(energy_data_complete$per_capita_electricity > 0),
          all(energy_data_complete$gdp > 0))

clean_energy_data <- energy_data_complete %>%
  mutate(
    # GDP per person: total GDP alone would mostly measure country size.
    gdp_per_capita = gdp / population,
    # Fossil share of electricity (coal + gas + oil).
    fossil_share_elec = coal_share_elec + gas_share_elec + oil_share_elec,
    # Low-carbon share: 100 minus fossil works because the shares were shown to sum to 100.
    low_carbon_share_elec = 100 - fossil_share_elec,
    # "New" renewables, which grew fastest in this period.
    wind_solar_share_elec = wind_share_elec + solar_share_elec,
    # World Bank region from the ISO code. Regions group energy-relevant economies
    # (e.g. the Gulf states) better than continents do.
    region = countrycode(iso_code, origin = "iso3c", destination = "region"),
    # Largest generation source in each country-year.
    # max.col() returns the column index of the row maximum; it is vectorised, so
    # it is faster and clearer than looping over rows with apply().
    dominant_source = str_remove(
      electricity_share_columns[max.col(as.matrix(pick(all_of(electricity_share_columns))),
                                        ties.method = "first")],
      "_share_elec"),
    # Log10 versions of the right-skewed variables. Log10 rather than ln because
    # each unit is a factor of 10, which is easy to read on a chart.
    log10_energy_per_capita = log10(energy_per_capita),
    log10_gdp_per_capita = log10(gdp_per_capita),
    log10_electricity_per_capita = log10(per_capita_electricity)
  )

# Data contracts: every country got a region, and the size rule
# (>= 3000 rows, >= 7 columns) from the brief is met.
stopifnot(!anyNA(clean_energy_data$region))
stopifnot(nrow(clean_energy_data) >= 3000, ncol(clean_energy_data) >= 7)

cat("CLEAN dataset:", nrow(clean_energy_data), "rows x", ncol(clean_energy_data),
    "columns,", n_distinct(clean_energy_data$country), "countries\n")
glimpse(clean_energy_data)


# ---- 1.10 Outliers: errors or genuine extremes? -------------------------------

# Flags values beyond 1.5 x IQR from the quartiles (Tukey's rule).
flag_iqr_outliers <- function(values) {
  quartiles <- quantile(values, probs = c(0.25, 0.75))
  interquartile_range <- quartiles[2] - quartiles[1]
  values < quartiles[1] - 1.5 * interquartile_range |
    values > quartiles[2] + 1.5 * interquartile_range
}

# Sample skewness, written out to avoid adding a package for one formula.
compute_skewness <- function(values) {
  mean((values - mean(values))^3) / sd(values)^3
}

skewed_variables <- c("energy_per_capita", "gdp_per_capita", "per_capita_electricity", "population")

# Outlier count and skewness for each variable, on the raw and the log10 scale.
# map_*() runs the same calculation for each variable name (no copy-pasting).
outlier_summary <- tibble(variable = skewed_variables) %>%
  mutate(
    outliers_raw_scale = map_int(variable, ~ sum(flag_iqr_outliers(clean_energy_data[[.x]]))),
    outliers_log_scale = map_int(variable, ~ sum(flag_iqr_outliers(log10(clean_energy_data[[.x]])))),
    skewness_raw = map_dbl(variable, ~ compute_skewness(clean_energy_data[[.x]])),
    skewness_log = map_dbl(variable, ~ compute_skewness(log10(clean_energy_data[[.x]])))
  )
print(outlier_summary)

# Which countries are the raw-scale outliers for energy use per person?
energy_outlier_countries <- clean_energy_data %>%
  filter(flag_iqr_outliers(energy_per_capita)) %>%
  count(country, sort = TRUE)
print(head(energy_outlier_countries, 12))

# Decision: KEEP the outliers. Iceland (aluminium smelting on cheap geothermal
# and hydro power), Qatar and Kuwait (oil and gas economies) and Norway are real
# energy systems, not typing errors. On the log scale they are no longer
# outliers, which shows the "outliers" come from skew, not bad data.
# Removing them would remove the most interesting cases for Section 2.

# Visual proof: the same variable on the raw and the log10 scale.
outlier_scale_plot <- clean_energy_data %>%
  transmute(`Raw (kWh per person)` = energy_per_capita,
            `Log10 (kWh per person)` = log10_energy_per_capita) %>%
  pivot_longer(everything(), names_to = "scale", values_to = "value") %>%
  # Explicit factor levels put the raw scale on the left (otherwise the order would be alphabetical).
  mutate(scale = factor(scale, levels = c("Raw (kWh per person)", "Log10 (kWh per person)"))) %>%
  ggplot(aes(x = scale, y = value)) +
  geom_boxplot(outlier.colour = "firebrick", fill = "grey90") +
  facet_wrap(~ scale, scales = "free") +
  labs(title = "Energy use per person: outliers disappear on a log scale",
       subtitle = "The extremes come from skew, not from data errors",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_blank())
save_figure(outlier_scale_plot, "03_outliers_raw_vs_log.png", width_in = 8, height_in = 4.5)

# Data-quality anomaly: gas power plants emit ~490 gCO2e/kWh over their
# lifecycle (IPCC, 2014). A grid that is mostly gas should therefore be well
# below a coal-heavy grid such as Poland (~950). These rows break that rule.
implausible_intensity_rows <- clean_energy_data %>%
  filter(gas_share_elec > 70, carbon_intensity_elec > 1000) %>%
  count(country, name = "years_flagged")
print(implausible_intensity_rows)
# These rows are kept but flagged (they are the source data as published).
# They reappear as extreme points in the EDA and on PC2.


# =============================================================================
# 2. EXPLORATORY DATA ANALYSIS + UNUSUAL PATTERNS  (Criterion 2)
# =============================================================================

# One cross-section year, used where repeated years would hide the pattern.
latest_year_data <- clean_energy_data %>% filter(year == 2022)


# ---- Q1: How is energy use per person distributed, and how does it vary by region? ----

energy_by_region_plot <- ggplot(clean_energy_data,
                                aes(x = reorder(region, energy_per_capita, FUN = median),
                                    y = energy_per_capita)) +
  geom_boxplot(fill = "steelblue", alpha = 0.4, outlier.alpha = 0.3) +
  # Log axis: the spread runs from ~100 to over 200,000 kWh per person.
  scale_y_log10(labels = scales::label_comma()) +
  coord_flip() +
  labs(title = "Q1. Energy use per person by region (2000-2022)",
       subtitle = "More than a 100-fold gap between regions; log scale",
       x = NULL, y = "Primary energy per person (kWh, log scale)")
save_figure(energy_by_region_plot, "04_q1_energy_by_region.png")


# ---- Q2: Does a richer country use more energy, and who breaks the trend? ----

# Fit the log-log relationship. The slope is an elasticity: % change in energy
# use for a 1 % change in GDP per person.
wealth_energy_model <- lm(log10_energy_per_capita ~ log10_gdp_per_capita, data = latest_year_data)
print(summary(wealth_energy_model))

# Residual = how far above or below the trend a country sits.
latest_year_with_residuals <- latest_year_data %>%
  mutate(energy_residual = residuals(wealth_energy_model))

# The 5 most extreme countries on each side of the trend get labels.
trend_breakers <- bind_rows(
  slice_max(latest_year_with_residuals, energy_residual, n = 5),
  slice_min(latest_year_with_residuals, energy_residual, n = 5)
)
print(select(trend_breakers, country, gdp_per_capita, energy_per_capita, energy_residual))

wealth_energy_plot <- ggplot(latest_year_with_residuals,
                             aes(x = gdp_per_capita, y = energy_per_capita)) +
  geom_point(aes(colour = region), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, colour = "black", linewidth = 0.7) +
  geom_text_repel(data = trend_breakers, aes(label = country), size = 3) +
  scale_x_log10(labels = scales::label_dollar()) +
  scale_y_log10(labels = scales::label_comma()) +
  labs(title = "Q2. Wealth and energy use move together (2022)",
       subtitle = paste0("Log-log slope = ", round(coef(wealth_energy_model)[2], 2),
                         ", R² = ", round(summary(wealth_energy_model)$r.squared, 2),
                         ". Labelled: largest deviations from the trend"),
       x = "GDP per person (2011 int-$, log)", y = "Energy per person (kWh, log)",
       colour = "Region")
save_figure(wealth_energy_plot, "05_q2_wealth_vs_energy.png", width_in = 9, height_in = 5.5)


# ---- Covariation overview: correlation matrix --------------------------------

correlation_columns <- c("log10_gdp_per_capita", "log10_energy_per_capita",
                         "log10_electricity_per_capita", "carbon_intensity_elec",
                         electricity_share_columns)

# Spearman rather than Pearson: it uses ranks, so it is robust to outliers and
# to the many zero shares, and it captures any monotonic relationship.
correlation_matrix <- cor(clean_energy_data[correlation_columns], method = "spearman")

# corrplot draws with base graphics, so open a png device, draw, then close it.
png("figures/06_correlation_heatmap.png", width = 2400, height = 2400, res = 300)
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", number.cex = 0.6, tl.cex = 0.7, tl.col = "black",
         title = "Spearman correlations (2000-2022)", mar = c(0, 0, 2, 0))
dev.off()

# Scatter-plot matrix of the main continuous variables, coloured by region.
pairs_plot <- latest_year_data %>%
  select(log10_gdp_per_capita, log10_energy_per_capita, carbon_intensity_elec,
         low_carbon_share_elec, region) %>%
  ggpairs(columns = 1:4, aes(colour = region, alpha = 0.6),
          upper = list(continuous = wrap("cor", size = 2.5, method = "spearman")),
          title = "Pairwise relationships (2022)")
save_figure(pairs_plot, "07_pairs_plot.png", width_in = 10, height_in = 8)


# ---- Q3: How has the electricity mix shifted since 2000? ---------------------

# Average share per region per year. Each country counts equally (not weighted
# by generation), so this tracks what a typical country does, not world totals.
mix_trend_by_region <- clean_energy_data %>%
  group_by(region, year) %>%
  summarise(fossil = mean(fossil_share_elec),
            `wind + solar` = mean(wind_solar_share_elec),
            .groups = "drop") %>%
  pivot_longer(c(fossil, `wind + solar`), names_to = "source_group", values_to = "mean_share")

mix_trend_plot <- ggplot(mix_trend_by_region, aes(year, mean_share, colour = region)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ source_group, scales = "free_y") +
  labs(title = "Q3. Wind + solar took off after ~2010; fossil shares barely moved",
       x = "Year", y = "Mean share of electricity (%)", colour = "Region")
save_figure(mix_trend_plot, "08_q3_mix_trend.png", width_in = 10, height_in = 5)

# Median country in 2000, 2010 and 2022, as numbers for the poster.
print(clean_energy_data %>%
        filter(year %in% c(2000, 2010, 2022)) %>%
        group_by(year) %>%
        summarise(median_fossil = median(fossil_share_elec),
                  median_low_carbon = median(low_carbon_share_elec),
                  median_wind_solar = median(wind_solar_share_elec)))


# ---- Q4: Do richer countries have cleaner electricity? (the unusual pattern) ----

# The rank correlation answers the question in one number each.
wealth_clean_correlations <- latest_year_data %>%
  summarise(
    rho_gdp_vs_low_carbon = cor(gdp_per_capita, low_carbon_share_elec, method = "spearman"),
    rho_gdp_vs_wind_solar = cor(gdp_per_capita, wind_solar_share_elec, method = "spearman")
  )
print(wealth_clean_correlations)

wealth_clean_plot <- ggplot(latest_year_data,
                            aes(gdp_per_capita, low_carbon_share_elec)) +
  geom_point(aes(colour = dominant_source), size = 2, alpha = 0.8) +
  # LOESS (local smoothing) instead of a straight line: the pattern is U-shaped, not linear.
  geom_smooth(method = "loess", se = FALSE, colour = "black", linewidth = 0.7) +
  geom_text_repel(data = filter(latest_year_data, low_carbon_share_elec > 90),
                  aes(label = country), size = 2.6, max.overlaps = 20) +
  scale_x_log10(labels = scales::label_dollar()) +
  labs(title = "Q4. Wealth does NOT predict clean electricity (2022)",
       subtitle = "Both the poorest and the richest countries reach >90 % low-carbon, by different routes",
       x = "GDP per person (log)", y = "Low-carbon share of electricity (%)",
       colour = "Dominant source")
save_figure(wealth_clean_plot, "09_q4_wealth_vs_low_carbon.png", width_in = 9, height_in = 5.5)

# The route matters: hydro for poorer countries, wind + solar for richer ones.
route_by_wealth <- latest_year_data %>%
  mutate(income_quartile = cut(gdp_per_capita,
                               breaks = quantile(gdp_per_capita, probs = seq(0, 1, 0.25)),
                               labels = c("Q1 poorest", "Q2", "Q3", "Q4 richest"),
                               include.lowest = TRUE)) %>%
  select(income_quartile, hydro_share_elec, wind_solar_share_elec, nuclear_share_elec) %>%
  pivot_longer(-income_quartile, names_to = "source", values_to = "share") %>%
  mutate(source = str_remove(source, "_share_elec"))

route_plot <- ggplot(route_by_wealth, aes(income_quartile, share, fill = source)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(title = "Q4b. Different routes to clean power by income quartile (2022)",
       x = "GDP-per-person quartile", y = "Share of electricity (%)", fill = "Source")
save_figure(route_plot, "10_q4b_route_by_income.png", width_in = 9, height_in = 5)


# ---- Q5: Which countries decarbonised, and which went backwards? -------------

# Low-carbon share in 2000 and 2022, side by side, for countries with both years.
low_carbon_change <- clean_energy_data %>%
  filter(year %in% c(2000, 2022)) %>%
  select(country, year, low_carbon_share_elec) %>%
  pivot_wider(names_from = year, values_from = low_carbon_share_elec, names_prefix = "share_") %>%
  drop_na() %>%
  mutate(change_points = share_2022 - share_2000)

# The 8 biggest gains and the 8 biggest losses.
biggest_movers <- bind_rows(slice_max(low_carbon_change, change_points, n = 8),
                            slice_min(low_carbon_change, change_points, n = 8)) %>%
  # Order countries by change, so the chart reads from biggest loss to biggest gain.
  mutate(country = fct_reorder(country, change_points))
print(biggest_movers)

# Dumbbell chart: a segment from the 2000 value to the 2022 value.
movers_plot <- ggplot(biggest_movers, aes(y = country)) +
  geom_segment(aes(x = share_2000, xend = share_2022, yend = country,
                   colour = change_points > 0), linewidth = 1.2) +
  geom_point(aes(x = share_2000), colour = "grey50", size = 2) +
  geom_point(aes(x = share_2022), colour = "black", size = 2) +
  scale_colour_manual(values = c(`TRUE` = "seagreen", `FALSE` = "firebrick"),
                      labels = c(`TRUE` = "Gained", `FALSE` = "Lost")) +
  labs(title = "Q5. Biggest changes in low-carbon share, 2000 (grey) to 2022 (black)",
       subtitle = "Several African countries lost ground as gas and oil met fast-growing demand",
       x = "Low-carbon share of electricity (%)", y = NULL, colour = NULL)
save_figure(movers_plot, "11_q5_biggest_movers.png", width_in = 9, height_in = 6)


# ---- Unusual pattern: carbon intensity that does not match the fuel mix ------

intensity_plot <- ggplot(latest_year_data, aes(fossil_share_elec, carbon_intensity_elec)) +
  geom_point(aes(colour = dominant_source), alpha = 0.8) +
  geom_text_repel(data = filter(latest_year_data, carbon_intensity_elec > 1000 |
                                  (fossil_share_elec > 95 & carbon_intensity_elec < 500)),
                  aes(label = country), size = 2.8) +
  labs(title = "Unusual pattern: grid carbon intensity vs fossil share (2022)",
       subtitle = "Gas-based Turkmenistan and Uzbekistan sit above coal-based grids: a data-quality flag",
       x = "Fossil share of electricity (%)", y = "Carbon intensity (gCO2e/kWh)",
       colour = "Dominant source")
save_figure(intensity_plot, "12_unusual_carbon_intensity.png", width_in = 9, height_in = 5.5)


# =============================================================================
# 3. ENCODING SCHEMES + PCA  (Criterion 3)
# =============================================================================

# ---- 3.1 One-hot encoding of a NOMINAL variable: region ----------------------

# Region has no natural order. Label encoding (1, 2, 3, ...) would invent one:
# the model would read "Sub-Saharan Africa (7)" as greater than "East Asia (1)"
# and treat the gap between codes as a distance. One-hot encoding makes one 0/1
# column per region instead, so no order or distance is implied.
encoded_energy_data <- clean_energy_data %>%
  dummy_cols(select_columns = "region", remove_selected_columns = FALSE) %>%
  # Turn names like "region_Sub-Saharan Africa" into "region_sub_saharan_africa".
  rename_with(~ str_replace_all(str_to_lower(.x), "[^a-z0-9]+", "_"), starts_with("region_"))

region_dummy_columns <- names(encoded_energy_data)[str_starts(names(encoded_energy_data), "region_")]
cat("One-hot columns created:", length(region_dummy_columns), "\n")
print(region_dummy_columns)

# Data contract: each row belongs to exactly one region (row sum = 1).
# This is also why one dummy must be dropped in a linear regression: the full
# set adds up to the intercept column (the "dummy-variable trap").
stopifnot(all(rowSums(encoded_energy_data[region_dummy_columns]) == 1))


# ---- 3.2 Label (ordinal) encoding of an ORDINAL variable: energy-use tier ----

# Energy-use tier (Low < Lower-middle < Upper-middle < High) has a real order,
# so mapping it to the integers 1-4 keeps information instead of inventing it.
# Breaks are the quartiles of energy use per person, giving four groups of equal size.
energy_tier_breaks <- quantile(encoded_energy_data$energy_per_capita, probs = seq(0, 1, 0.25))

encoded_energy_data <- encoded_energy_data %>%
  mutate(
    energy_use_tier = cut(energy_per_capita, breaks = energy_tier_breaks,
                          labels = c("Low", "Lower-middle", "Upper-middle", "High"),
                          include.lowest = TRUE, ordered_result = TRUE),
    # as.integer() on an ordered factor returns its level position: Low = 1 ... High = 4.
    energy_use_tier_code = as.integer(energy_use_tier)
  )

# Show the mapping: each label appears with exactly one code.
print(count(encoded_energy_data, energy_use_tier, energy_use_tier_code))

# Preview of the encoded columns, for a poster code snippet.
print(head(select(encoded_energy_data, country, year, region, all_of(region_dummy_columns[1:3]),
                  energy_use_tier, energy_use_tier_code)))


# ---- 3.3 PCA: choose and standardise the features ----------------------------

# Twelve continuous features describing each country-year's energy system.
# The log10 versions are used for the skewed variables (see 1.10), so a few
# extreme countries do not dominate the principal components.
# fossil, low-carbon and wind + solar shares are left out: they are exact sums
# of the eight source shares and add no new information.
pca_feature_columns <- c(electricity_share_columns,
                         "log10_energy_per_capita", "log10_gdp_per_capita",
                         "log10_electricity_per_capita", "carbon_intensity_elec")

pca_input_data <- encoded_energy_data[pca_feature_columns]

# scale. = TRUE standardises every feature (mean 0, SD 1). Without it, carbon
# intensity (range 0-1300) would swamp the log variables (range ~2-5) purely
# because of its units. center = TRUE subtracts the mean, which PCA requires.
energy_pca <- prcomp(pca_input_data, center = TRUE, scale. = TRUE)
print(summary(energy_pca))

# The sign of a principal component is arbitrary (PC and -PC explain the same
# variance). Fix the direction so the reading is stable:
# PC1 points towards "richer", PC2 towards "more carbon-intensive".
orient_component <- function(pca_object, component_index, anchor_variable) {
  if (pca_object$rotation[anchor_variable, component_index] < 0) {
    pca_object$rotation[, component_index] <- -pca_object$rotation[, component_index]
    pca_object$x[, component_index] <- -pca_object$x[, component_index]
  }
  pca_object
}
energy_pca <- orient_component(energy_pca, 1, "log10_gdp_per_capita")
energy_pca <- orient_component(energy_pca, 2, "carbon_intensity_elec")


# ---- 3.4 How many components to keep? ----------------------------------------

# Eigenvalue = variance captured by each component (the squared standard deviation).
pca_eigenvalues <- energy_pca$sdev^2

variance_table <- tibble(
  component = paste0("PC", seq_along(pca_eigenvalues)),
  eigenvalue = pca_eigenvalues,
  variance_pct = 100 * pca_eigenvalues / sum(pca_eigenvalues),
  cumulative_pct = cumsum(variance_pct)
)
print(variance_table, n = Inf)

# Two standard retention rules, so the choice is justified, not just eyeballed.
components_by_kaiser <- sum(pca_eigenvalues > 1)
components_for_80_pct <- which(variance_table$cumulative_pct >= 80)[1]
cat("Kaiser rule (eigenvalue > 1) keeps", components_by_kaiser, "components\n")
cat("80 % cumulative-variance rule keeps", components_for_80_pct, "components\n")

# The last eigenvalue is ~0: the eight shares always add up to 100 %, so one of
# them can be computed from the others. PCA detects this redundancy itself.
cat("Smallest eigenvalue:", signif(min(pca_eigenvalues), 3), "\n")

scree_plot <- fviz_eig(energy_pca, addlabels = TRUE, ncp = length(pca_eigenvalues)) +
  geom_hline(yintercept = 100 / length(pca_eigenvalues), linetype = "dashed", colour = "red") +
  labs(title = "Scree plot: variance explained by each principal component",
       subtitle = "Dashed line = average variance per component (Kaiser rule on standardised data)")
save_figure(scree_plot, "13_pca_scree.png")


# ---- 3.5 Interpret the components through their loadings ---------------------

# Loadings = the weight of each original feature in each component.
pca_loadings <- as_tibble(energy_pca$rotation[, 1:4], rownames = "feature")
print(pca_loadings %>% mutate(across(where(is.numeric), ~ round(.x, 2))))

loadings_heatmap <- pca_loadings %>%
  pivot_longer(-feature, names_to = "component", values_to = "loading") %>%
  ggplot(aes(component, feature, fill = loading)) +
  geom_tile() +
  geom_text(aes(label = round(loading, 2)), size = 3) +
  # Diverging palette: red = positive weight, blue = negative weight.
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick", midpoint = 0) +
  labs(title = "PCA loadings: what each component measures", x = NULL, y = NULL)
save_figure(loadings_heatmap, "14_pca_loadings.png", width_in = 7, height_in = 6)

# Variable correlation circle: arrows that point the same way are positively correlated.
variable_circle <- fviz_pca_var(energy_pca, col.var = "contrib", repel = TRUE,
                                gradient.cols = c("grey70", "steelblue", "firebrick")) +
  labs(title = "Variables on PC1-PC2")
save_figure(variable_circle, "15_pca_variable_circle.png", width_in = 7, height_in = 6)


# ---- 3.6 Where do countries sit on the components? ---------------------------

# Attach the first four component scores to each country-year.
pca_scores <- encoded_energy_data %>%
  select(country, year, region, dominant_source) %>%
  bind_cols(as_tibble(energy_pca$x[, 1:4]))

# Use 2022 only for a readable map of countries (one point per country).
pca_scores_latest <- pca_scores %>% filter(year == 2022)

# Label the 4 most extreme countries at each end of PC1 and PC2.
extreme_countries <- bind_rows(
  slice_max(pca_scores_latest, PC1, n = 4), slice_min(pca_scores_latest, PC1, n = 4),
  slice_max(pca_scores_latest, PC2, n = 4), slice_min(pca_scores_latest, PC2, n = 4)
) %>% distinct(country, .keep_all = TRUE)
print(select(extreme_countries, country, PC1, PC2))

pca_country_map <- ggplot(pca_scores_latest, aes(PC1, PC2, colour = region)) +
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_vline(xintercept = 0, colour = "grey80") +
  geom_point(alpha = 0.8) +
  geom_text_repel(data = extreme_countries, aes(label = country), size = 3, show.legend = FALSE) +
  labs(title = "Countries on the first two principal components (2022)",
       x = paste0("PC1 (", round(variance_table$variance_pct[1], 1), "%): development / energy intensity"),
       y = paste0("PC2 (", round(variance_table$variance_pct[2], 1), "%): carbon-intensive vs hydro & wind"),
       colour = "Region")
save_figure(pca_country_map, "16_pca_country_map.png", width_in = 9, height_in = 6)


# ---- 3.7 Using the one-hot matrix: mean component score per region -----------

# Region dummies (0/1 matrix) times the scores gives the sum of scores per
# region; dividing by the column sums (countries per region) gives the means.
# This shows what the one-hot encoding is for: it acts as a group-membership matrix.
region_indicator_matrix <- as.matrix(encoded_energy_data[region_dummy_columns])
region_mean_scores <- (t(region_indicator_matrix) %*% energy_pca$x[, 1:2]) /
  colSums(region_indicator_matrix)
print(round(region_mean_scores, 2))


# ---- 3.8 Sensitivity check: what if the dummies go into the PCA? -------------

# Adding 0/1 region columns to PCA is debatable: once standardised, binary
# columns get the same weight as the continuous features and pull components
# towards "which region" rather than "what kind of energy system".
# This compares the two versions to support keeping them out.
pca_with_dummies <- prcomp(cbind(pca_input_data, encoded_energy_data[region_dummy_columns]),
                           center = TRUE, scale. = TRUE)
variance_comparison <- tibble(
  version = c("Energy features only", "Energy features + region dummies"),
  features = c(ncol(pca_input_data), ncol(pca_input_data) + length(region_dummy_columns)),
  pc1_pct = c(summary(energy_pca)$importance[2, 1], summary(pca_with_dummies)$importance[2, 1]) * 100,
  pc1_pc2_pct = c(summary(energy_pca)$importance[3, 2], summary(pca_with_dummies)$importance[3, 2]) * 100
)
print(variance_comparison)


# =============================================================================
# 4. SAVE OUTPUTS
# =============================================================================

# Clean dataset (with encodings), so the results can be reproduced without re-running Section 1.
write_csv(encoded_energy_data, "data/owid_energy_clean_encoded.csv")
# Component scores, for any later analysis.
write_csv(pca_scores, "data/pca_scores.csv")

cat("\nDone. Figures in ./figures, data in ./data\n")

# Package versions, so the analysis can be reproduced.
sessionInfo()

