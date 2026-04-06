#### Install packages. ####
install.packages("haven")
install.packages("dplyr")
install.packages("stringr")
install.packages("arrow")
install.packages("purrr")
install.packages("tidyr")
install.packages("R.utils")


# ============================================================
# BRFSS Summaries 1990–2024 (Pre + Post + Combined)
# - Single pass over each XPT (no re-reading)
# - Auto-find files; dedupe per year by largest file
# - Optional parallel processing on macOS/Linux
# - Safe writing with auto-created output dir
# ============================================================


# ==== 1) Clean filenames (remove trailing spaces/hidden chars, keep .XPT) ====
suppressPackageStartupMessages({
  library(stringr); library(dplyr); library(tibble); library(readr); library(haven)
})

DATA_DIR <- file.path(
  "/Users","franklina_addae","Downloads",
  "Master's Project 2025","BRFS Data","xpt data files"
)
stopifnot(dir.exists(DATA_DIR))

all_paths <- list.files(DATA_DIR, full.names = TRUE, recursive = TRUE, include.dirs = FALSE)
if (!length(all_paths)) stop("Folder is empty: ", DATA_DIR)

sanitize_name <- function(p) {
  d <- dirname(p); n <- basename(p); n0 <- n
  # remove non-printable chars
  n <- gsub("[^[:print:]]", "", n, perl = TRUE)
  # collapse and trim whitespace
  n <- gsub("\\s+", " ", n, perl = TRUE)
  n <- trimws(n)
  # if .xpt appears with junk after, keep up to .xpt
  if (grepl("(?i)\\.xpt", n, perl = TRUE)) {
    n <- sub("(?i)(.*?\\.xpt).*", "\\1", n, perl = TRUE)
  }
  # normalize extension to .XPT
  n <- sub("(?i)\\.xpt$", ".XPT", n, perl = TRUE)
  
  new_p <- file.path(d, n)
  if (identical(p, new_p)) return(NULL)
  
  # avoid overwriting existing files
  if (file.exists(new_p)) {
    stem <- sub("(?i)\\.xpt$", "", n, perl = TRUE)
    k <- 1
    repeat {
      cand <- file.path(d, sprintf("%s_%d.XPT", stem, k))
      if (!file.exists(cand)) { new_p <- cand; break }
      k <- k + 1
    }
  }
  
  ok <- file.rename(p, new_p)
  tibble(old = p, new = new_p, renamed = ok)
}

# Only target suspicious or possibly bad .XPT names
bad_candidates <- basename(all_paths)
bad_idx <- grepl("[^\\x20-\\x7E]", bad_candidates) |         # hidden chars
  grepl("\\.XPT\\s+$", bad_candidates, perl=TRUE) | # trailing space after .XPT
  grepl("(?i)\\.xpt.+$", bad_candidates, perl=TRUE) # junk after .xpt

renames <- bind_rows(lapply(all_paths[bad_idx], sanitize_name))
print(renames, n = 100)

# ==== 2) Re-scan XPTs and summarize 1990–2024 ====
extract_year <- function(f) {
  yy <- regmatches(basename(f), regexpr("(19\\d{2}|20\\d{2})", basename(f)))
  ifelse(length(yy) == 1, as.integer(yy), NA_integer_)
}

xpt_all <- list.files(DATA_DIR, pattern = "(?i)\\.xpt$", full.names = TRUE, recursive = TRUE)
yrs_all <- vapply(xpt_all, extract_year, integer(1))
keep    <- !is.na(yrs_all) & yrs_all >= 1990 & yrs_all <= 2024
xpt_all <- xpt_all[keep]; yrs_all <- yrs_all[keep]

# dedupe per year by largest file
sizes <- file.info(xpt_all)$size
ord   <- order(yrs_all, -sizes)
xpt_all <- xpt_all[ord]; yrs_all <- yrs_all[ord]
dedup  <- !duplicated(yrs_all)
xpt    <- xpt_all[dedup]; yrs <- yrs_all[dedup]

message("Final span after cleaning: ", ifelse(length(yrs), paste0(min(yrs),"–",max(yrs)), "none"))
stopifnot(length(yrs) > 0)

process_one <- function(path, yr) {
  df <- haven::read_xpt(path)
  n_nonmiss <- sum(vapply(df, function(x) any(!is.na(x)), logical(1)))
  list(year=yr,
       sample_size=nrow(df),
       n_columns_total=ncol(df),
       n_variables_any_nonmissing=n_nonmiss,
       varnames=names(df))
}

res_list <- lapply(seq_along(xpt), function(i) process_one(xpt[i], yrs[i]))

per_year <- tibble(
  year = vapply(res_list, `[[`, integer(1), "year"),
  sample_size = vapply(res_list, `[[`, numeric(1), "sample_size"),
  n_columns_total = vapply(res_list, `[[`, integer(1), "n_columns_total"),
  n_variables_any_nonmissing = vapply(res_list, `[[`, integer(1), "n_variables_any_nonmissing")
) |> arrange(year)

vars_by_year <- lapply(res_list, `[[`, "varnames")
names(vars_by_year) <- per_year$year
all_union <- length(unique(unlist(vars_by_year, use.names = FALSE)))
pre_idx   <- which(per_year$year <= 2013)
post_idx  <- which(per_year$year >= 2014)
pre_union  <- if (length(pre_idx))  length(unique(unlist(vars_by_year[pre_idx],  use.names = FALSE))) else 0L
post_union <- if (length(post_idx)) length(unique(unlist(vars_by_year[post_idx], use.names = FALSE))) else 0L

preACA  <- dplyr::filter(per_year, year <= 2013)
postACA <- dplyr::filter(per_year, year >= 2014)

totals <- tibble(
  years_in_dataset               = nrow(per_year),
  total_sample_all_years         = sum(per_year$sample_size, na.rm = TRUE),
  total_variables_union_all_years= all_union,
  preACA_sample_le_2013          = sum(preACA$sample_size, na.rm = TRUE),
  preACA_variables_union         = pre_union,
  postACA_sample_ge_2014         = sum(postACA$sample_size, na.rm = TRUE),
  postACA_variables_union        = post_union
)

OUT_DIR <- file.path(DATA_DIR, "summaries"); dir.create(OUT_DIR, showWarnings=FALSE, recursive=TRUE)
readr::write_csv(per_year, file.path(OUT_DIR, "brfss_per_year_counts.csv"))
readr::write_csv(totals,   file.path(OUT_DIR, "brfss_totals.csv"))
message("Wrote summaries to: ", normalizePath(OUT_DIR))




# ================================
# BRFSS plots for data description
# ================================

# 0) Packages (auto-install if missing)
pkgs <- c("readr","dplyr","ggplot2","scales")
need <- setdiff(pkgs, rownames(installed.packages()))
if (length(need)) install.packages(need, quiet = TRUE)
library(readr); library(dplyr); library(ggplot2); library(scales)

# 1) Point to your per-year CSV (EDIT this path if needed)
per_year_csv <- "/Users/franklina_addae/Downloads/Master's Project 2025/BRFS Data/xpt data files/summaries/brfss_per_year_counts.csv"

# 2) Read and tidy
df <- read_csv(per_year_csv, show_col_types = FALSE) |>
  mutate(
    year = as.integer(year),
    sample_size = as.numeric(sample_size),
    n_columns_total = as.numeric(n_columns_total)
  ) |>
  arrange(year)

stopifnot(all(c("year","sample_size","n_columns_total") %in% names(df)))

# 3) Output folder (next to CSV)
out_dir <- file.path(dirname(per_year_csv), "figs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 4) Figure 1 — Sample size by year
p1 <- ggplot(df, aes(x = year, y = sample_size)) +
  geom_line(linewidth = 1.1, color = "#E69F00") +
  geom_vline(xintercept = 2013.5, linetype = "dashed") +
  scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 2)) +
  scale_y_continuous(labels = comma) +
  labs(title = "BRFSS Sample Size by Year",
       x = "Year", y = "Sample size") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

f1 <- file.path(out_dir, "brfss_sample_size_trend.png")
ggsave(f1, p1, width = 12, height = 7, dpi = 300)

# 5) Figure 2 — Number of variables by year
p2 <- ggplot(df, aes(x = year, y = n_columns_total)) +
  geom_line(linewidth = 1.1, color = "#E69F00") +
  geom_vline(xintercept = 2013.5, linetype = "dashed") +
  scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 2)) +
  labs(title = "BRFSS: Number of Variables by Year",
       x = "Year", y = "Number of variables") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

f2 <- file.path(out_dir, "brfss_variables_trend.png")
ggsave(f2, p2, width = 12, height = 7, dpi = 300)

cat("Saved:\n", f1, "\n", f2, "\n")




## =======================
## BRFSS "safe-mode" tonight runner
## Structure + simple patterns (no survey design)
## =======================

#### Install packages. ####
install.packages("haven")
install.packages("dplyr")
install.packages("stringr")
install.packages("readr")
install.packages("ggplot2")
install.packages("scales")



suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(stringr)
  library(readr)
  library(ggplot2)
  library(scales)
})

## ---- 0) POINT THIS TO YOUR XPT FOLDER (EDIT THIS ONE LINE ONLY) ----
DATA_DIR <- file.path(
  "/Users","franklina_addae","Downloads",
  "Master's Project 2025","BRFS Data","xpt data files"
)

## ---- 1) Find .XPT files robustly (tolerate trailing spaces, any case) ----
all_files <- list.files(DATA_DIR, full.names = TRUE, recursive = TRUE)
is_xpt <- grepl("(?i)\\.xpt\\s*$", all_files)
xpt_files <- all_files[is_xpt]

if (!length(xpt_files)) stop("No XPT files found under: ", DATA_DIR)

year_from_name <- function(path) {
  y <- str_extract(basename(path), "(19\\d{2}|20\\d{2})")
  ifelse(is.na(y), NA_integer_, as.integer(y))
}
years <- vapply(xpt_files, year_from_name, integer(1))
keep <- !is.na(years) & years >= 1990 & years <= 2024
xpt_files <- xpt_files[keep]
years     <- years[keep]
ord <- order(years)
xpt_files <- xpt_files[ord]; years <- years[ord]

message("Files detected: ", length(xpt_files), " (", paste(range(years), collapse="–"), ")")

## ---- 2) Output dirs ----
OUT_DIR <- file.path(DATA_DIR, "tonight_outputs")
FIG_DIR <- file.path(OUT_DIR, "figs")
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

## ---- 3) Quick helpers ----
safe_xpt <- function(f) tryCatch(read_xpt(f), error = function(e) NULL)

guess_weight <- function(df) {
  cands <- c("_LLCPWT","X_LLCPWT","_FINALWT","X_FINALWT","FINALWT")
  hit <- cands[cands %in% names(df)]
  ifelse(length(hit)>0, hit[1], NA_character_)
}
guess_strata <- function(df) {
  cands <- c("_STSTR","X_STSTR","STSTR")
  hit <- cands[cands %in% names(df)]
  ifelse(length(hit)>0, hit[1], NA_character_)
}
guess_psu <- function(df) {
  cands <- c("_PSU","X_PSU","PSU")
  hit <- cands[cands %in% names(df)]
  ifelse(length(hit)>0, hit[1], NA_character_)
}

## Robust binary recoder for “has coverage” (HLTHPLN… variants)
pick_first <- function(nm_vec, df) {
  hit <- nm_vec[nm_vec %in% names(df)]
  ifelse(length(hit)>0, hit[1], NA_character_)
}
recode_yesno <- function(x, yes = 1, no = 2) {
  # handle labelled, factors, characters
  if (inherits(x, "labelled")) x <- haven::zap_labels(x)
  xc <- suppressWarnings(as.numeric(as.character(x)))
  if (all(is.na(xc))) {
    xx <- tolower(as.character(x))
    out <- ifelse(xx %in% c("yes","y","1","true"), 1L,
                  ifelse(xx %in% c("no","n","0","false"), 0L, NA_integer_))
  } else {
    out <- ifelse(xc %in% yes, 1L, ifelse(xc %in% no, 0L, NA_integer_))
  }
  out
}

## ---- 4) Structure scan per file ----
struct_list <- vector("list", length(xpt_files))
for (i in seq_along(xpt_files)) {
  f <- xpt_files[i]; y <- years[i]
  message("Reading year ", y, " …")
  df <- safe_xpt(f)
  if (is.null(df)) {
    struct_list[[i]] <- tibble(year = y, file = f, n = NA_integer_, p = NA_integer_,
                               weight = NA_character_, strata = NA_character_, psu = NA_character_)
    next
  }
  struct_list[[i]] <- tibble(
    year   = y,
    file   = f,
    n      = nrow(df),
    p      = ncol(df),
    weight = guess_weight(df),
    strata = guess_strata(df),
    psu    = guess_psu(df)
  )
}
struct_df <- bind_rows(struct_list) %>% arrange(year)

# Save structure + a per-year summary
write_csv(struct_df, file.path(OUT_DIR, "structure_all_years.csv"))
per_year <- struct_df %>% group_by(year) %>%
  summarize(sample_size = first(n), n_vars = first(p), .groups="drop")
write_csv(per_year, file.path(OUT_DIR, "per_year_counts.csv"))

## ---- 5) Pattern plots (sample size & number of variables) ----
p1 <- ggplot(per_year, aes(year, sample_size)) +
  geom_line(linewidth=1) + geom_point() +
  geom_vline(xintercept = 2013.5, linetype="dashed") +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(title = "BRFSS Sample Size by Year", x = "Year", y = "Sample size") +
  theme_minimal(base_size = 13)
ggsave(file.path(FIG_DIR, "sample_size_trend.png"), p1, width=10, height=6, dpi=300)

p2 <- ggplot(per_year, aes(year, n_vars)) +
  geom_line(linewidth=1) + geom_point() +
  geom_vline(xintercept = 2013.5, linetype="dashed") +
  labs(title = "Number of Variables by Year", x = "Year", y = "Variables") +
  theme_minimal(base_size = 13)
ggsave(file.path(FIG_DIR, "variables_trend.png"), p2, width=10, height=6, dpi=300)

## ---- 6) Simple pattern: % insured (UNWEIGHTED, robust to name shifts) ----
coverage_candidates <- c("HLTHPLN1","HLTHPLN","COVERED","COVERAGE","HAVEHCOV")

ser_list <- vector("list", length(xpt_files))
for (i in seq_along(xpt_files)) {
  f <- xpt_files[i]; y <- years[i]
  df <- safe_xpt(f)
  if (is.null(df)) {
    ser_list[[i]] <- tibble(year=y, var=NA_character_, mean=NA_real_, n=NA_integer_)
    next
  }
  v <- pick_first(coverage_candidates, df)
  if (is.na(v)) {
    ser_list[[i]] <- tibble(year=y, var=NA_character_, mean=NA_real_, n=NA_integer_)
    next
  }
  x <- recode_yesno(df[[v]], yes=1, no=2)
  ser_list[[i]] <- tibble(
    year=y, var=v,
    mean = mean(x, na.rm=TRUE),
    n    = sum(!is.na(x))
  )
}
ser_ins <- bind_rows(ser_list) %>% arrange(year)
write_csv(ser_ins, file.path(OUT_DIR, "series_insured_unweighted.csv"))

p3 <- ggplot(ser_ins, aes(year, mean)) +
  geom_line(linewidth=1, na.rm=TRUE) + geom_point(na.rm=TRUE) +
  geom_vline(xintercept = 2013.5, linetype="dashed") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0,1)) +
  labs(title="Share with any health coverage (unweighted)", x="Year", y="Share") +
  theme_minimal(base_size = 13)
ggsave(file.path(FIG_DIR, "insured_trend_unweighted.png"), p3, width=10, height=6, dpi=300)

message("✅ Done. Outputs in: ", OUT_DIR)






## ============================================================
## CHAPTER 4 – CLEAN, LIGHT SCRIPT (FRANKLINA)
## ============================================================

## ------------------------------------------------------------
## 0) Packages  (install once, then keep commented)
## ------------------------------------------------------------

# pkgs <- c(
#   "tidyverse","janitor","scales","readr","haven",
#   "survey","srvyr",
#   "did","fixest",
#   "gt",
#   "DoubleML","mlr3","mlr3learners","glmnet","ranger"
# )
# install.packages(setdiff(pkgs, installed.packages()[,"Package"]))

library(tidyverse)
library(janitor)
library(scales)
library(readr)
library(haven)
library(survey)
library(srvyr)
library(did)
library(fixest)
library(gt)
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(glmnet)
library(ranger)

theme_set(theme_minimal(base_size = 12))
update_geom_defaults("line",  list(linewidth = 0.9))
update_geom_defaults("point", list(size = 2.2))


## ------------------------------------------------------------
## 1) Paths & data import  (YOUR REAL PATHS)
## ------------------------------------------------------------

csv_dir <- "~/Downloads/Master's Project 2025/BRFS Data/Raw Data Set/2012/tonight_outputs"
rds_dir <- "~"   # home folder where the RDS files live

# CSVs from your “safe-mode” script
per_year <- read_csv(file.path(csv_dir, "per_year_counts.csv")) |>
  clean_names()     # year, sample_size, n_vars (check with names(per_year))

vars_raw <- read_csv(file.path(csv_dir, "structure_all_years.csv")) |>
  clean_names()     # year, file, n, p, ...

cov_raw  <- read_csv(file.path(csv_dir, "series_insured_unweighted.csv")) |>
  clean_names()     # year, var, mean, n

# RDS objects from earlier steps
brfss     <- readRDS(file.path(rds_dir, "brfss_clean.rds"))
dat       <- readRDS(file.path(rds_dir, "dat_ready.rds"))
dat_clean <- readRDS(file.path(rds_dir, "dat_clean.rds"))
des       <- readRDS(file.path(rds_dir, "survey_design.rds"))
did_df    <- readRDS(file.path(rds_dir, "did_df.rds"))

# Output folder for Chapter 4 figures/tables (optional)
out_dir <- file.path(csv_dir, "outputs_ch4")
dir.create(out_dir, showWarnings = FALSE)


## ------------------------------------------------------------
## 2) Structure & “first look” figures
##    Figure 1: sample size by year
##    Figure 2: number of variables by year
## ------------------------------------------------------------

# ---- Figure 1: BRFSS sample size by year ----

# Load packages
library(ggplot2)
library(dplyr)
library(scales)

glimpse(per_year)

# UNC color palette
unc_blue  <- "#003865"
unc_gold  <- "#FFC627"
dark_gold <- "#C99700"
soft_gray <- "grey50"

# Plot
p_sample <- ggplot(per_year, aes(x = year, y = sample_size)) +
  
  # ✅ Post-2014 shaded region (policy period)
  annotate("rect",
           xmin = 2014, xmax = max(per_year$year),
           ymin = -Inf, ymax = Inf,
           alpha = 0.04, fill = unc_gold) +
  
  # Bars
  geom_col(fill = unc_blue, width = 0.7) +
  
  # Reference lines (IMPROVED VISIBILITY)
  geom_vline(xintercept = 2011,
             linetype = "dashed",
             color = dark_gold,
             linewidth = 1.2) +
  
  geom_vline(xintercept = 2014,
             linetype = "solid",
             color = unc_gold,
             linewidth = 1.4) +
  
  # Optional label for 2014 (very nice for poster)
  annotate("text",
           x = 2014,
           y = max(per_year$sample_size),
           label = "Medicaid Expansion",
           color = unc_blue,
           vjust = -0.5,
           size = 4) +
  
  # Labels
  labs(
    title = "BRFSS Sample Size by Year",
    subtitle = "2011 redesign and 2014 Medicaid expansion",
    x = NULL,
    y = "Number of Respondents"
  ) +
  
  # Scales
  scale_y_continuous(labels = label_comma()) +
  
  # Theme (poster-ready)
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = unc_blue),
    plot.subtitle = element_text(size = 11),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Display plot
p_sample 

# Save for poster
#ggsave("Rplot01_unc.png", p_sample, width = 8, height = 5, dpi = 300)

# Optional save:
# ggsave(file.path(out_dir, "fig1_sample_size.png"), p_sample,
#        width = 8, height = 4.8, dpi = 300)



# ---- Figure 2: Number of variables by year ----

# Load packages
library(ggplot2)
library(dplyr)
library(scales)

# Prepare data
vars <- vars_raw |>
  group_by(year) |>
  summarize(n_vars = first(p), .groups = "drop")

# UNC color palette
unc_blue  <- "#003865"
unc_gold  <- "#FFC627"
dark_gold <- "#C99700"

# Plot
p_vars <- ggplot(vars, aes(x = year, y = n_vars)) +
  
  # Post-2011 shaded region
  annotate("rect",
           xmin = 2011, xmax = max(vars$year),
           ymin = -Inf, ymax = Inf,
           alpha = 0.04, fill = unc_gold) +
  
  # Main line and points
  geom_line(color = unc_blue, linewidth = 1.2) +
  geom_point(color = unc_gold, size = 3) +
  
  # Reference line
  geom_vline(xintercept = 2011,
             linetype = "dashed",
             color = dark_gold,
             linewidth = 1.2) +
  
  # Optional label
  annotate("text",
           x = 2011,
           y = max(vars$n_vars),
           label = "2011 redesign",
           color = unc_blue,
           vjust = -0.5,
           hjust = -0.05,
           size = 4) +
  
  # Labels
  labs(
    title = "Number of Variables Captured by Year",
    subtitle = "Rise before 2011, then stabilization after the BRFSS redesign",
    x = NULL,
    y = "Variable Count"
  ) +
  
  # Scales
  scale_x_continuous(
    breaks = seq(min(vars$year), max(vars$year), by = 2)
  ) +
  
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = unc_blue),
    plot.subtitle = element_text(size = 11),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Display plot
p_vars

# Save for poster
#ggsave("Rplot02_unc.png", p_vars, width = 8, height = 5, dpi = 300)

# Optional save:
# ggsave(file.path(out_dir, "fig2_num_variables.png"), p_vars,
#        width = 8, height = 4.8, dpi = 300)





## ------------------------------------------------------------
## 3) Descriptive outcome trend
##    Unweighted “any coverage” reconnaissance line (Figure 3)
## ------------------------------------------------------------


# Install once if needed
install.packages("ggplot2")
install.packages("dplyr")
install.packages("scales")

# Load packages
library(ggplot2)
library(dplyr)
library(scales)

# Prepare data
cov <- cov_raw |>
  rename(
    share_insured = mean,
    n_nonmissing  = n
  ) |>
  filter(!is.na(share_insured))

min(cov$year)

# UNC-inspired colors
unc_blue <- "#003865"
unc_gold <- "#FFC627"
soft_gray <- "grey50"

# Plot
p_cov <- ggplot(cov, aes(x = year, y = share_insured)) +
  geom_line(color = unc_blue, linewidth = 1.2) +
  geom_point(color = unc_gold, size = 3) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = soft_gray, linewidth = 0.8) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_x_continuous(
    breaks = seq(min(cov$year), max(cov$year), by = 2)
  ) +
  labs(
    title = "Any Health Coverage",
    subtitle = "Unweighted reconnaissance series; dashed line marks 2014 Medicaid expansion",
    x = NULL,
    y = "Share insured"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = unc_blue),
    plot.subtitle = element_text(size = 11),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

p_cov


  
# Optional save:
# ggsave(file.path(out_dir, "fig3_coverage_unweighted.png"), p_cov,
#        width = 8, height = 4.8, dpi = 300)




## ------------------------------------------------------------
## 4) Weighted descriptives by Medicaid expansion status
##    Table 1
## ------------------------------------------------------------

library(dplyr)
library(gt)
library(scales)

options(survey.lonely.psu = "adjust")

tab_by_treat <- dat_clean %>%
  group_by(treat) %>%
  summarize(
    n = n(),
    checkup = if (all(is.na(checkup12m))) NA_real_
    else stats::weighted.mean(checkup12m == 1, w = wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    treat = if_else(treat == 1, "Expansion States", "Non-Expansion States")
  )

# UNC colors
unc_blue   <- "#003865"
unc_gold   <- "#FFC627"
light_gold <- "#FFF4CC"
light_blue <- "#EAF1F7"

tab1_gt <- gt(tab_by_treat) |>
  fmt_number(columns = n, decimals = 0, use_seps = TRUE) |>
  fmt_percent(
    columns = checkup,
    decimals = 1
  ) |>
  cols_label(
    treat = "Group",
    n = "N",
    checkup = "Routine Checkup"
  ) |>
  tab_header(
    title = md("**Weighted Descriptives by Medicaid Expansion Status**"),
    subtitle = "BRFSS 2011–2024"
  ) |>
  tab_style(
    style = list(
      cell_fill(color = unc_blue),
      cell_text(color = "white", weight = "bold", size = px(16))
    ),
    locations = cells_title(groups = "title")
  ) |>
  tab_style(
    style = list(
      cell_fill(color = light_blue),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) |>
  tab_style(
    style = cell_fill(color = light_gold),
    locations = cells_body(
      rows = treat == "Expansion States"
    )
  ) |>
  opt_row_striping() |>
  tab_options(
    table.font.size = px(13),
    heading.align = "center",
    column_labels.font.size = px(13),
    data_row.padding = px(6),
    table.border.top.color = unc_blue,
    table.border.bottom.color = unc_blue,
    heading.border.bottom.color = unc_blue,
    column_labels.border.bottom.color = unc_blue,
    row.striping.background_color = "#F8FAFC"
  )

tab1_gt




# Optional export:
# gtsave(tab1_gt, filename = file.path(out_dir, "table1_descriptives.png"))
# write_csv(tab_by_treat, file.path(out_dir, "table1_descriptives.csv"))




## ------------------------------------------------------------
## 5) Trend plots: routine checkup by expansion status
##    Figure 4
## ------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(scales)
library(readr)
library(janitor)

out_dir <- getwd()
state_year_path <- file.path(out_dir, "state_year_checkup_means.csv")

if (file.exists(state_year_path)) {
  # FAST: just read the saved file
  state_year <- read_csv(state_year_path) |>
    clean_names()
} else {
  # FASTER: use dat_clean + weighted.mean instead of srvyr::survey_mean
  state_year <- dat_clean %>%
    group_by(state, year, treat) %>%
    summarize(
      mean_checkup = if (all(is.na(checkup12m))) NA_real_
      else stats::weighted.mean(checkup12m == 1, w = wt, na.rm = TRUE),
      .groups = "drop"
    )
  
  write_csv(state_year, state_year_path)
}

trend_df <- state_year %>%
  group_by(year, treat) %>%
  summarize(
    y = mean(mean_checkup, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(treat == 1, "Expansion states", "Non-expansion states")
  )


# UNC color palette
unc_blue  <- "#003865"
unc_gold  <- "#FFC627"
dark_gold <- "#C99700"
soft_gray <- "grey50"

p_trend <- ggplot(trend_df, aes(x = year, y = y, color = group)) +
  
  # Light post-2014 shading
  annotate("rect",
           xmin = 2014, xmax = max(trend_df$year),
           ymin = -Inf, ymax = Inf,
           alpha = 0.04, fill = unc_gold) +
  
  # Lines and points
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.8) +
  
  # 2014 reference line
  geom_vline(xintercept = 2014,
             linetype = "dashed",
             color = dark_gold,
             linewidth = 1.2) +
  
  # Optional label for 2014
  annotate("text",
           x = 2014,
           y = max(trend_df$y, na.rm = TRUE),
           label = "Medicaid Expansion",
           color = unc_blue,
           vjust = -0.5,
           hjust = -0.05,
           size = 4) +
  
  # Colors for groups
  scale_color_manual(
    values = c(
      "Expansion states" = unc_blue,
      "Non-expansion states" = dark_gold
    )
  ) +
  
  # Axes
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  scale_x_continuous(
    breaks = seq(min(trend_df$year), max(trend_df$year), by = 1)
  ) +
  
  # Labels
  labs(
    title = "Routine Checkup in Past Year",
    subtitle = "Weighted trends by Medicaid expansion status",
    x = NULL,
    y = "Share with routine checkup",
    color = NULL
  ) +
  
  # Theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = unc_blue),
    plot.subtitle = element_text(size = 11),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

p_trend

# Save for poster
ggsave("Rplot06_unc.png", p_trend, width = 8, height = 5, dpi = 300)

# Optional save:
# ggsave(file.path(out_dir, "fig4_trend_checkup.png"), p_trend,
#        width = 8, height = 4.8, dpi = 300)




## ------------------------------------------------------------
## 6) Staggered DiD – FAST VERSION using state-year averages
## ------------------------------------------------------------

# We start from dat_clean (has wt, state, year, treat, checkup12m, adopt_year)

# 6A. Build state-year level dataset
state_year_did <- dat_clean %>%
  group_by(state, year, adopt_year, treat) %>%
  summarize(
    y = if (all(is.na(checkup12m))) NA_real_
    else stats::weighted.mean(checkup12m == 1, w = wt, na.rm = TRUE),
    w_cell = sum(wt, na.rm = TRUE),  # total survey weight in that state-year
    .groups = "drop"
  ) %>%
  # Drop rows with missing outcome
  filter(!is.na(y)) %>%
  # Build variables in the format att_gt expects
  transmute(
    y = y,
    t = as.integer(year),
    g = if_else(is.na(adopt_year), 0L, as.integer(adopt_year)),  # 0 = never-treated
    w = w_cell,
    state = as.integer(state)
  )

# Quick sanity check: how big is this now?
dim(state_year_did)
dplyr::count(state_year_did, t)  # number of states per year

# 6B. Run Callaway–Sant’Anna on the aggregated data
att <- att_gt(
  yname   = "y",
  tname   = "t",
  gname   = "g",
  idname  = "state",          # panel index for repeated state observations
  panel   = FALSE,            # still repeated cross-sections, but at state level
  data    = state_year_did,
  control_group = "notyettreated",
  weightsname   = "w",        # state-year total weight as cell weight
  clustervars   = "state"
)

summary(att)

# 6C. Overall ATT and dynamic effects
agg_overall <- aggte(att, type = "simple",  na.rm = TRUE)
agg_dyn     <- aggte(att, type = "dynamic", na.rm = TRUE)

summary(agg_overall)
summary(agg_dyn)

overall_tbl <- tibble(
  overall_ATT = agg_overall$overall.att,
  overall_SE  = agg_overall$overall.se
)
overall_tbl

# Optional save:
# write_csv(overall_tbl, file.path(out_dir, "did_overall_att.csv"))



library(ggplot2)

# Extract event-study data
es_df <- data.frame(
  event_time = agg_dyn$egt,
  att = agg_dyn$att.egt,
  se = agg_dyn$se.egt
)

# Confidence intervals
es_df$lower <- es_df$att - 1.96 * es_df$se
es_df$upper <- es_df$att + 1.96 * es_df$se

# UNC colors
unc_blue <- "#003865"
unc_gold <- "#FFC627"

p_es_clean <- ggplot(es_df, aes(x = event_time, y = att)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = unc_gold, alpha = 0.25) +
  geom_line(color = unc_blue, linewidth = 1.2) +
  geom_point(color = unc_blue, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = unc_gold, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  labs(
    title = "Event Study: Medicaid Expansion and Routine Checkups",
    x = "Years Relative to Expansion",
    y = "Treatment Effect"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", color = unc_blue),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank()
  )

p_es_clean

# Optional save:
# ggsave(file.path(out_dir, "fig5_event_study.png"), p_es,
#        width = 8, height = 4.8, dpi = 300)




## ------------------------------------------------------------
## 7) Double Machine Learning (DML) cross-check – OPTIONAL
##    Wrapped in if (FALSE) so it will NOT run unless you choose
## ------------------------------------------------------------

if (FALSE) {
  set.seed(123)
  
  dml_df <- dat_clean %>%
    transmute(
      y = as.numeric(checkup12m == 1),
      d = as.integer(treat == 1),
      sex,
      age,
      educ,
      income,
      coverage,
      year,
      state
    ) %>%
    tidyr::drop_na()
  
  # Subsample to keep it manageable
  dml_df_small <- dml_df %>%
    dplyr::sample_n(size = min(100000, n()))
  
  X <- dml_df_small %>%
    select(sex, age, educ, income, coverage, year, state) %>%
    as.data.frame()
  
  d <- dml_df_small$d
  y <- dml_df_small$y
  
  dml_data_raw <- data.frame(y = y, d = d, X)
  
  ml_l <- lrn("regr.glmnet")                           # outcome model
  ml_m <- lrn("classif.ranger", predict_type = "prob") # treatment model
  
  dml_data <- DoubleMLData$new(
    data   = dml_data_raw,
    y_col  = "y",
    d_cols = "d"
  )
  
  dml_plr <- DoubleMLPLR$new(
    data   = dml_data,
    ml_g   = ml_l,
    ml_m   = ml_m,
    n_folds = 5
  )
  
  dml_plr$fit()
  dml_plr$summary()
  
  dml_ate <- tibble(
    ATE = dml_plr$coef,
    SE  = dml_plr$se
  )
  
  dml_ate
  
  # Optional:
  # write_csv(dml_ate, file.path(out_dir, "dml_ate_checkup.csv"))
}





