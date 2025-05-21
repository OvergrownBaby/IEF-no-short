#!/usr/bin/env Rscript
# ©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤
#  Collector ¨C LS & LO frontiers + two ¦Ã-indifference curves (¦Ã = 10)
#  Usage: Rscript plot.R [mode]  (mode = smoke, mini, or prod; default = prod)
# ©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤©¤
suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)   # ggplot2, dplyr, tidyr, purrr, readr
})

## 0 ? process command line arguments -----------------------------------------
args <- commandArgs(trailingOnly = TRUE)
MODE <- if (length(args) > 0) tolower(args[1]) else "prod"

# Validate mode
if (!MODE %in% c("smoke", "mini", "prod")) {
  stop("Invalid mode. Please use 'smoke', 'mini', or 'prod'")
}

cat("Collecting and plotting data for mode:", MODE, "\n")

## 1 ? locate point files for specified mode ----------------------------------
ief_root <- "Data/Generated/Portfolios/IEF_PML"

pts <- list.files(
          ief_root,
          pattern   = "frontier_point\\.csv$",
          recursive = TRUE,
          full.names = TRUE
        ) |>
        keep(~ grepl(paste0("_", MODE), .x)) |>  # Filter by specified mode
        map_dfr(readr::read_csv, show_col_types = FALSE) |>
        arrange(constraint, gamma_rel)

if (nrow(pts) == 0) {
  stop("No data files found for mode: ", MODE)
}

cat("Found", nrow(pts), "data points for mode:", MODE, "\n")

## 2 ? choose the fixed ¦Ã for indifference curves -----------------------------
GAMMA_TARGET <- 10          # ¦Ã value used in the published Panel A

ref <- pts |>
       filter(gamma_rel == GAMMA_TARGET) |>
       transmute(
         constraint,
         gamma  = gamma_rel,
         sigma0 = sigma,
         mu0    = r_tc,
         C      = mu0 - 0.5 * gamma * sigma0^2
       )

# Check if GAMMA_TARGET exists in the data
if (nrow(ref) == 0) {
  warning("Gamma target ", GAMMA_TARGET, " not found in the data. Using highest available gamma.")
  highest_gamma <- max(pts$gamma_rel)
  cat("Using gamma =", highest_gamma, "instead\n")
  
  ref <- pts |>
         filter(gamma_rel == highest_gamma) |>
         transmute(
           constraint,
           gamma  = gamma_rel,
           sigma0 = sigma,
           mu0    = r_tc,
           C      = mu0 - 0.5 * gamma * sigma0^2
         )
}

## 3 ? build ¦Ì = C + ? ¦Ã ¦Ò? for each constraint ------------------------------
sigma_grid <- seq(0, 1.05 * max(pts$sigma), length.out = 200)

indiff <- ref |>
  mutate(curve_id = constraint) |>
  rowwise() |>
  mutate(curve = list(
           tibble(
             sigma    = sigma_grid,
             mu       = C + 0.5 * gamma * sigma^2,
             curve_ids = curve_id          # tag for grouping
           )
         )) |>
  unnest(curve) |>
  ungroup()

## 4 ? plot -------------------------------------------------------------------
p <- ggplot() +
       ## dashed iso-utility curves ¡ª **two separate groups**
       geom_line(
         data      = indiff,
         aes(x = sigma, y = mu, group = curve_id),   # ¡û group!
         colour    = "grey60",
         linetype  = "dashed"
       ) +
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
         subtitle = "(long-short vs long-only)",
         x     = "Volatility σ (annualised)",
         y     = "Excess return μ net of TC (annualised)"
       ) +
       theme_bw(base_size = 11) +
       theme(legend.position = "bottom")

## 5 ? save artefacts ---------------------------------------------------------
png_name <- paste0("PortfolioML_frontiers_", MODE, "_with_IC_gamma", GAMMA_TARGET, ".png")
csv_name <- paste0("PortfolioML_frontiers_", MODE, ".csv")

ggsave(png_name, p, width = 8, height = 6)
fwrite(pts, csv_name)

cat("?  wrote ", png_name, " and ", csv_name, "\n", sep = "")