#!/usr/bin/env Rscript
# ─────────────────────────────────────────────────────────────────────────────
#  Portfolio-ML: long-short vs. long-only  —  ONE gamma PER RUN
#  honours IEF_MODE = {smoke, mini, prod}
# ─────────────────────────────────────────────────────────────────────────────

if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
  RhpcBLASctl::blas_set_num_threads(26)
  RhpcBLASctl::omp_set_num_threads(26)
}

suppressPackageStartupMessages({
  library(nnls); library(tidyverse); library(data.table)
  library(tictoc); library(lubridate)
})

source("Main.R")                      # loads settings, pf_set, features …

# ─────────────────────── 1  CONFIGURATION ──────────────────────────────────
## gamma passed through the environment (one gamma per job)
if (!nzchar(Sys.getenv("GAMMA")))
  stop("env-var GAMMA missing - launch via launch_no_short.sh")
gamma_val             <- as.numeric(Sys.getenv("GAMMA"))
stopifnot(is.finite(gamma_val), gamma_val > 0)
settings$ef$gamma_rel <- pf_set$gamma_rel <- gamma_val   # length-one

## which “mode” (size): smoke | mini | prod   – passed as IEF_MODE
MODE         <- tolower(Sys.getenv("IEF_MODE", "prod"))
IS_SMOKETEST <- MODE == "smoke"
IS_MINI      <- MODE == "mini"
IS_PROD      <- MODE == "prod"
run_sub      <- IS_SMOKETEST           # Prepare-Data.R looks at this flag

## mode-specific tweaks *only* for speed / memory
if (IS_SMOKETEST) {
  settings$screens$size_screen <- "perc_low50_high100_min50"
  pf_set$wealth                <- 1e10
  settings$pf_ml$p_vec         <- 2^5
  settings$cov_set$industries  <- FALSE
  if (Sys.getenv("SUBSET_N") == "") Sys.setenv(SUBSET_N = "1000")
}
if (IS_MINI) {
  settings$screens$size_screen <- "perc_low20_high100_min150"
  pf_set$wealth                <- 1e10
  settings$pf_ml$p_vec         <- 2^7
  settings$cov_set$industries  <- FALSE
}

if (IS_PROD) {
  settings$screens$size_screen <- "all"
}

## force SINGLE wealth level
pf_set$wealth <- pf_set$wealth[1]

# ─────────────────────── 2  OUTPUT FOLDER ──────────────────────────────────
out_dir <- file.path("Data/Generated/Portfolios",
                     sprintf("IEF_PML/WEALTH%.0f_GAMMA%.3g_%s",
                             pf_set$wealth, gamma_val, MODE))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
message("  Output   ", normalizePath(out_dir))

options(echo = TRUE, warn = 1)

# ════════════════════════════════════════════════════════════════════════════
# 1  Data preparation
# ════════════════════════════════════════════════════════════════════════════
tic("Total run time")
tic("Data preparation")
source("1 - Prepare Data.R",               echo = FALSE)   # uses run_sub flag
source("3 - Estimate Covariance Matrix.R", echo = FALSE)
source("4 - Prepare Portfolio Data.R",     echo = FALSE)
if (!"pred_ld1" %in% names(chars)) chars[, pred_ld1 := 0]
toc()

# ════════════════════════════════════════════════════════════════════════════
# 2  AIM weights (Portfolio-ML)
# ════════════════════════════════════════════════════════════════════════════
tic("AIM weight recursion")
pfml <- pfml_implement(
  data_tc     = chars,
  cov_list    = barra_cov,
  lambda_list = lambda_list,
  features    = features,
  risk_free   = risk_free,
  wealth      = wealth,
  mu          = pf_set$mu,
  gamma_rel   = gamma_val,
  dates_full  = dates_m2,
  dates_oos   = dates_oos,
  lb          = pf_set$lb_hor,
  hp_years    = hp_years,
  rff_feat    = TRUE,
  g_vec       = settings$pf_ml$g_vec,
  p_vec       = settings$pf_ml$p_vec,
  l_vec       = settings$pf_ml$l_vec,
  scale       = settings$pf_ml$scale,
  orig_feat   = settings$pf_ml$orig_feat,
  iter        = 10,
  seed        = settings$seed_no,
  balanced    = FALSE
)
saveRDS(pfml$aims, file = file.path(out_dir, "pfml_aims.RDS"))
toc()

# ════════════════════════════════════════════════════════════════════════════
# 3  Weight recursion  (Long-short  &  Long-only)
# ════════════════════════════════════════════════════════════════════════════
tic("Weight iteration (LS & LO)")
weights_ls <- chars[eom %in% dates_oos & valid == TRUE] %>%
  pfml_w(dates        = dates_oos,
         cov_list     = barra_cov,
         lambda_list  = lambda_list,
         gamma_rel    = gamma_val,
         iter         = 10,
         risk_free    = risk_free,
         wealth       = wealth,
         mu           = pf_set$mu,
         aims         = pfml$aims)

weights_lo <- chars[eom %in% dates_oos & valid == TRUE] %>%
  pfml_w_no_short(dates          = dates_oos,
                  cov_list       = barra_cov,
                  lambda_list    = lambda_list,
                  gamma_rel      = gamma_val,
                  iter           = 10,
                  risk_free      = risk_free,
                  wealth         = wealth,
                  mu             = pf_set$mu,
                  aims           = pfml$aims,
                  use_projection = TRUE)
toc()

# # ════════════════════════════════════════════════════════════════════════════
# # 4  Summarise → one point per variant
# # ════════════════════════════════════════════════════════════════════════════
# sum_point <- function(w_tbl, tag) {
#   w_tbl %>%
#     pf_ts_fun(data = chars, wealth = wealth, gam = gamma_val) %>%
#     summarise(
#       gamma_rel  = gamma_val,
#       wealth_end = pf_set$wealth,
#       constraint = tag,
#       r     = mean(r)  * 12,
#       sd    = sd(r)    * sqrt(12),
#       tc    = mean(tc) * 12,
#       r_tc  = mean(r - tc) * 12,
#       sr    = mean(r - tc) / sd(r) * sqrt(12),
#       obj   = (mean(r) - 0.5 * var(r) * gamma_val - mean(tc)) * 12
#     )
# }

# ─────────────────────────────────────────────────────────────
# 4  Summarise → one point per variant      (add diagnostics)
# ─────────────────────────────────────────────────────────────

check_finite <- function(x, tag) {
  bad <- which(!is.finite(x$r))
  if (length(bad)) {
    cat(sprintf(
      "[non-finite]  %-10s  %d bad value(s)  rows: %s\n",
      tag, length(bad), paste(head(bad, 5), collapse = ", ")
    ))
    print(x[rindex := .I][bad, .(rindex, eom, r)][1:min(5, .N)])
  } else {
    cat(sprintf("[non-finite]  %-10s  none\n", tag))
  }
}

check_finite(weights_ls, "long_short")
check_finite(weights_lo, "long_only")
# sum_point <- function(w_tbl, tag) {

#   ts <- w_tbl %>%                            # monthly P&L path
#         pf_ts_fun(data   = chars,
#                    wealth = wealth,
#                    gam    = gamma_val)

#   ## ── five-line quick check ────────────────────────────────
#   cat(sprintf("[diag]  %-10s  obs=%4d   NA(r)=%3d   first=%s   last=%s\n",
#               tag,
#               nrow(ts),
#               sum(is.na(ts$r)),
#               format(min(ts$eom_ret)),
#               format(max(ts$eom_ret))))
#   ## ─────────────────────────────────────────────────────────

#   ts %>%
#     summarise(
#       gamma_rel  = gamma_val,
#       wealth_end = pf_set$wealth,
#       constraint = tag,
#       r     = mean(r,  na.rm = TRUE) * 12,
#       sigma = sd(r,    na.rm = TRUE) * sqrt(12),   # ← renamed
#       tc    = mean(tc, na.rm = TRUE) * 12,
#       r_tc  = mean(r - tc, na.rm = TRUE) * 12,
#       sr    = mean(r - tc, na.rm = TRUE) /
#               sd(r, na.rm = TRUE) * sqrt(12),
#       obj   = (mean(r, na.rm = TRUE) -
#               0.5 * var(r, na.rm = TRUE) * gamma_val -
#               mean(tc, na.rm = TRUE)) * 12
#     )

# }

sum_point <- function(w_tbl, tag) {

  ts <- w_tbl %>%                      # monthly P&L path
        pf_ts_fun(data   = chars,
                   wealth = wealth,
                   gam    = gamma_val)

  ## ── 1 ? basic sanity -------------------------------------------------
  cat(sprintf("[diag-1]  %-10s  n=%4d  first=%s  last=%s\n",
              tag,
              nrow(ts),
              format(min(ts$eom_ret)),
              format(max(ts$eom_ret))))

  ## ── 2 ? compute σ outside dplyr --------------------------------------
  s  <- sd(ts$r, na.rm = TRUE) * sqrt(12)
  m  <- mean(ts$r, na.rm = TRUE) * 12

  cat(sprintf("[diag-2]  %-10s  mean(r)=% .4f   sd(r)=% .4f\n",
              tag, m, s))

  ## ── 3 ? (optional) print first few returns ---------------------------
  cat("[diag-3]  head(r): ", paste(head(round(ts$r, 6)), collapse = ", "),
      "\n")

  ## ── 4 ? return summary data-frame ------------------------------------
  data.table(
    gamma_rel  = gamma_val,
    wealth_end = pf_set$wealth,
    constraint = tag,
    r          = m,
    sigma      = s,
    tc         = mean(ts$tc, na.rm = TRUE) * 12,
    r_tc       = mean(ts$r - ts$tc, na.rm = TRUE) * 12,
    sr         = (mean(ts$r - ts$tc, na.rm = TRUE) /
                  sd(ts$r, na.rm = TRUE)) * sqrt(12),
    obj        = (mean(ts$r, na.rm = TRUE) -
                  0.5 * var(ts$r, na.rm = TRUE) * gamma_val -
                  mean(ts$tc, na.rm = TRUE)) * 12
  )
}



frontier_pt <- bind_rows(
  sum_point(weights_ls, "long_short"),
  sum_point(weights_lo, "long_only")
)

fwrite(frontier_pt, file.path(out_dir, "frontier_point.csv"))

cat("\nfinished   no_short.R   (gamma =", gamma_val, ", mode =", MODE, ")\n")
toc(); invisible()
