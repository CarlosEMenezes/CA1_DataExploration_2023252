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
devtools::install_github("r-lib/conflicted")
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


