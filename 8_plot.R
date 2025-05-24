#!/usr/bin/env Rscript
# ©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤
#  Collector ¨C LS & LO frontiers
#  Usage: Rscript plot.R [mode] [wealth]  (mode = smoke, mini, or prod; default = prod)
# ©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤
suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)   # ggplot2, dplyr, tidyr, purrr, readr
})

## 0 ? process command line arguments -----------------------------------------
args <- commandArgs(trailingOnly = TRUE)
MODE <- if (length(args) > 0) tolower(args[1]) else "prod"
WEALTH <- if (length(args) > 1) as.numeric(args[2]) else 1e10

# Validate mode
if (!MODE %in% c("smoke", "mini", "prod")) {
  stop("Invalid mode. Please use 'smoke', 'mini', or 'prod'")
}

# Validate wealth
if (!is.finite(WEALTH) || WEALTH <= 0) {
  stop("Invalid wealth. Please provide a positive numeric value")
}

cat("Collecting and plotting data for mode:", MODE, "and wealth:", format(WEALTH, scientific = TRUE), "\n")

## 1 ? locate point files for specified mode and wealth -----------------------
ief_root <- "Data/Generated/Portfolios/IEF_PML"

# Debug: Show what we're looking for
wealth_pattern <- paste0("WEALTH", format(WEALTH, scientific = FALSE), "_")
mode_pattern <- paste0("_", MODE)
cat("Looking for wealth pattern:", wealth_pattern, "\n")
cat("Looking for mode pattern:", mode_pattern, "\n")

# Get all CSV files first
all_csv_files <- list.files(
  ief_root,
  pattern = "frontier_point\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

cat("Found", length(all_csv_files), "total CSV files\n")
if (length(all_csv_files) > 0) {
  cat("Example paths:\n")
  cat(paste(head(all_csv_files, 3), collapse = "\n"), "\n")
}

# Filter files
matching_files <- all_csv_files |>
  keep(~ grepl(wealth_pattern, .x) && grepl(mode_pattern, .x))

cat("Found", length(matching_files), "matching files\n")
if (length(matching_files) > 0) {
  cat("Matching files:\n")
  cat(paste(matching_files, collapse = "\n"), "\n")
}

if (length(matching_files) == 0) {
  stop("No matching CSV files found for mode: ", MODE, " and wealth: ", format(WEALTH, scientific = TRUE))
}

# Read and combine the CSV files
pts <- matching_files |>
  map_dfr(readr::read_csv, show_col_types = FALSE)

# Debug: Show columns and first few rows
cat("Columns in data:", paste(names(pts), collapse = ", "), "\n")
cat("Number of rows:", nrow(pts), "\n")

if (nrow(pts) == 0) {
  stop("No data found in CSV files")
}

# Now we can safely arrange
pts <- pts |> arrange(constraint, gamma_rel)

cat("Found", nrow(pts), "data points for mode:", MODE, "and wealth:", format(WEALTH, scientific = TRUE), "\n")

## 2 ? plot -------------------------------------------------------------------
p <- ggplot() +
       ## efficient frontiers
       geom_path(
         data  = pts,
         aes(x = sigma, y = r_tc,
             colour = constraint, group = constraint),
         linewidth = 1.1
       ) +
       geom_point(
         data  = pts,
         aes(x = sigma, y = r_tc,
             colour = constraint,
             shape  = factor(gamma_rel)),
         size  = 3
       ) +
       scale_colour_manual(
         values = c(long_short = "#2980b9",
                    long_only  = "#c0392b"),
         name   = "Variant"
       ) +
       scale_shape_discrete(name = "γ") +
       labs(
         title = paste0("Portfolio-ML implementable efficient frontiers (", toupper(MODE), " mode)"),
         subtitle = paste0("(long-short vs long-only, wealth = ", format(WEALTH, scientific = TRUE), ")"),
         x     = "Volatility σ (annualised)",
         y     = "Excess return μ net of TC (annualised)"
       ) +
       theme_bw(base_size = 11) +
       theme(legend.position = "bottom")

## 3 ? save artefacts ---------------------------------------------------------
png_name <- paste0("PortfolioML_frontiers_", MODE, "_wealth", format(WEALTH, scientific = FALSE), ".png")
csv_name <- paste0("PortfolioML_frontiers_", MODE, "_wealth", format(WEALTH, scientific = FALSE), ".csv")

ggsave(png_name, p, width = 8, height = 6)
fwrite(pts, csv_name)

cat("✓ wrote ", png_name, " and ", csv_name, "\n", sep = "")