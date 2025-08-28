library(tidyverse)
library(readr)
library(stringr)
library(scales)
library(sf)
library(tigris)
library(gt)
library(glue)
library(dplyr)
options(tigris_use_cache = TRUE) #later on I use tigris::state to make the pretty national map

# ==============================
# 0 - Files setup etc.
# ==============================
cfg <- list(
  PATH_ACS        = "acs_school_district_clean.csv", #from ACS Census table
  PATH_ENROLL     = "school_enrollment.csv", #from NCES 2023-2024
  PATH_FIN        = "districts_matched.csv", # also from ACS
  PATH_ACS_POP    = "acs_schooldistrict.txt", # ACS 
  PATH_HOTDAYS    = "FAST_hsd80_schoolyear_baseline_vs_2025_2050_ssp3_ssp5.csv", # i called this "fast" because i used really coarse data to get the general ideas. I can go back and get the finer granular stuff but the results should largely be the same? 
  PATH_FIPS_NAMES = "fips_state.csv", # I just made this manually in Excel            
  
  BENCH_Q      = 0.80, # This is the DPS benchmark I'm thinking - but we can tweak after discussing
  HOTSPOT_Q    = 0.90, # Here I am assuming the top 10% should be "hotspots" but idk right now
  MIN_SOCIAL_N = 3,    # At least three of the five variables should be counted (bc then there is too much noise in the social index)
  HIGH_HEAT_CUTOFF = 30,  # later on I see other days' cutoffs too but throughout 30 is used (SSP5)
  OUT_DIR      = "out" #just the folder i saved stuff in
)

# folders
dir.create(cfg$OUT_DIR, showWarnings = FALSE) #just the folder i saved stuff in
dir.create(file.path(cfg$OUT_DIR, "csv"),    showWarnings = FALSE, recursive = TRUE) #just the folder i saved stuff in
dir.create(file.path(cfg$OUT_DIR, "figs"),   showWarnings = FALSE, recursive = TRUE) #folder
dir.create(file.path(cfg$OUT_DIR, "tables"), showWarnings = FALSE, recursive = TRUE) #folder!!!

# =====================================================================
# 1. BASIC ADMIN STUFF
# ================================================================
pad7    <- function(x) str_pad(gsub("\\D","", as.character(x)), 7, pad="0") #so basically padding to 7 digits bc GeoIDs look like "0100010" 
safe_div<- function(n,d){ out <- n/d; out[!is.finite(out)] <- NA_real_; out } #whenever there is no data don't FREAK OUT, just add NA
make_z  <- function(x, reverse=FALSE, log=FALSE){ 
  if (all(is.na(x))) return(rep(NA_real_, length(x))) #again just add NA if there is nothing
  y <- if (log) log1p(x) else x; z <- as.numeric(scale(y)); if (reverse) -z else z #do a log transform if needed; standardize to Z score; flip the sign 
}

ensure_dps <- function(df){
  if (!all(c("debt_expanded","enrollment_total") %in% names(df))) return(df) #first checking if the columns exist in the df
  df %>% mutate(
    debt_per_student = if_else(enrollment_total > 0, debt_expanded/enrollment_total, NA_real_), #here I add a new column on DPS but if enrollment is 0 or NA then itll just say NA real. 
    debt_per_capita  = if ("population_acs" %in% names(.)) safe_div(debt_expanded, population_acs) else NA_real_ #here im adding DPC but only when we have pop data from ACS
  )
}
#=================
# 1.5 autosaving stuff
#=================
.save_path <- function(kind, stem, ext) file.path(cfg$OUT_DIR, kind, glue::glue("{stem}.{ext}")) #so basically itll save it like: "out/figs/plot_name_xyz.png"

save_plot <- function(p, stem, width=7, height=5, dpi=320){ #consistent sizes and dpi 
  p_png <- .save_path("figs", stem, "png")
  ggplot2::ggsave(p_png, p, width=width, height=height, dpi=dpi)
  invisible(p_png)
}

save_gt <- function(gtobj, stem){ # so this makes the prettiest tables the only thing is it'll save as html 
  htm <- .save_path("tables", stem, "html") #it'll be good for formatting consistency
  gt::gtsave(gtobj, htm)                          # always HTML <3 
  invisible(htm)
}

save_csv <- function(df, name) {
  readr::write_csv(df, file = file.path("out", "csv", paste0(name, ".csv")))
}

# ==============================
# 2. loading data
# ==============================
acs <- readr::read_csv(cfg$PATH_ACS, show_col_types = FALSE) %>% #the ACS file has all the social indicators
  dplyr::mutate(
    GeoId = pad7(if ("district_geoid" %in% names(.)) district_geoid else GeoId), #again the pad=7 because of GeoID 
    dplyr::across(
      c(median_household_income, unemployment_pct, uninsured_pct, snap_pct, renter_occ_pct),
      ~ suppressWarnings(readr::parse_number(as.character(.x)))
    )
  ) %>%
  dplyr::distinct(GeoId, .keep_all = TRUE)

enr <- readr::read_csv(cfg$PATH_ENROLL, show_col_types = FALSE) %>% #the enrollment stuff is separate so it's in this file 
  dplyr::transmute(
    GeoId = pad7(`Agency ID - NCES Assigned [District] Latest available year`), #this has a different primary key - it's NCES agency ID
    enrollment_total = suppressWarnings(as.numeric(`Total Students All Grades (Excludes AE) [District] 2023-24`)) 
  ) %>%
  dplyr::distinct(GeoId, .keep_all = TRUE)

fin <- readr::read_csv(cfg$PATH_FIN, show_col_types = FALSE) %>% #debt stuff 
  dplyr::mutate(GeoId = pad7(GeoId)) %>%
  dplyr::select(GeoId, debt_district_only, debt_expanded) %>%
  dplyr::distinct(GeoId, .keep_all = TRUE)

acs_pop <- readr::read_delim(cfg$PATH_ACS_POP, delim = "|", show_col_types = FALSE) %>% #this has population data
  dplyr::transmute( #it's different from enrollment data - this is more for the debt per capita calc and general regional population
    GeoId = pad7(GeoId),
    population_acs = suppressWarnings(as.numeric(DP05_1_EST))
  ) %>%
  dplyr::group_by(GeoId) %>%
  dplyr::summarise(population_acs = mean(population_acs, na.rm = TRUE), .groups = "drop")

hotdays <- readr::read_csv(cfg$PATH_HOTDAYS, show_col_types = FALSE) %>% #this is the quick and easy CMIP6 stuff - just tas data
  dplyr::mutate(GeoId = pad7(GeoId))

state_names <- readr::read_csv(cfg$PATH_FIPS_NAMES, show_col_types = FALSE, col_names = "raw") %>% #state stuff
  tidyr::separate(raw, into = c("state","state_name"), sep = ",", fill = "right", extra = "merge") %>%
  dplyr::filter(!state %in% c("state","STATE","State")) %>%
  dplyr::mutate(state = stringr::str_pad(stringr::str_trim(state), 2, pad = "0"), #keeping the pad 0, 2 digit FIPS 
                state_name = stringr::str_trim(state_name))

# ==============================
# 3) MERGE + INDEX
# ==============================
df_base <- acs %>%
  left_join(enr,     by="GeoId") %>% #merging it all on GEOID which was a journey 
  left_join(fin,     by="GeoId") %>%
  left_join(acs_pop, by="GeoId") %>%
  left_join(hotdays, by="GeoId") %>%
  ensure_dps() # this kept popping up as an issue so here we calculate DPS or DPC if the data is there 

# first i made sure the DPS and DPC exist before we make z fiscal
# earlier it was calculating it weirdly but now it seems to be okay........ 
df <- df_base %>%
  mutate(
    z_fiscal    = make_z(debt_per_student, reverse=TRUE, log=TRUE), # I did a log transformation and then reversed the sign  
    z_income    = make_z(median_household_income, reverse=TRUE), #no log here but reversed sign so that more disadvantage = bigger score
    z_unemp     = make_z(unemployment_pct), 
    z_uninsured = make_z(uninsured_pct),
    z_snap      = make_z(snap_pct),
    z_renter    = make_z(renter_occ_pct),
    z_social    = rowMeans(cbind(z_income, z_unemp, z_uninsured, z_snap, z_renter)), na.rm=TRUE, #making sure all NA values are removed first
    z_climate   = make_z(d_hsd80_2050_ssp5),
    
    # caps
    z_fiscal  = pmax(pmin(z_fiscal,  4), -4), # this is just so the plots look consistent 
    z_social  = pmax(pmin(z_social,  4), -4), # capping the z scores so it's on a pretty scale of -4 and 4
    z_climate = pmax(pmin(z_climate, 4), -4),
    
    # if there aren't at least 3 social indicators then drop z_social altogether
    n_social  = rowSums(!is.na(cbind(z_income, z_unemp, z_uninsured, z_snap, z_renter))),
    z_social  = if_else(n_social >= cfg$MIN_SOCIAL_N, z_social, NA_real_),
    
    # OUR INDEX <3 <3 
    triple_vulnerability = rowMeans(cbind(z_fiscal, z_social, z_climate), na.rm=TRUE), #again drop any NAs
    state = substr(GeoId, 1, 2) #the GEOID's first two digits are the state 
  )

df_strict <- df %>% filter(!is.na(z_fiscal), !is.na(z_social), !is.na(z_climate)) #so before the composite score had just 1-2 components sometimes
thr_tv    <- quantile(df_strict$triple_vulnerability, cfg$HOTSPOT_Q, na.rm=TRUE) #all this ensures the composite has all three components or it gets dropped
df_strict <- df_strict %>% mutate(hotspot = triple_vulnerability >= thr_tv) #i call it df STRICT 

readr::write_csv(df_strict, "out/csv/district_metrics_scored.csv") # has a Boolean column that says yes or no to hotspot

# ==============================
# 4) SUMMARY TABLES (auto-print + save)
# ==============================
# Top 50 triple-vulnerable
top50 <- df_strict %>%
  arrange(desc(triple_vulnerability)) %>%
  slice_head(n = 50)

top50 %>%  
  select(district_name, GeoId, enrollment_total,
         debt_per_student, z_fiscal, z_social, z_climate,
         triple_vulnerability, hotspot) %>%
  save_csv("top50_triple_vulnerability_districts_clean") # and then here i trim it to the 50 most vulnerable 


# State summary (CONUS only)
conus_drop <- c("02","11","15","72") #cutting these out from sensitivity analyses 
state_summary <- df_strict %>% #they kept skewing the results and they didnt match F33 totals by more than 10% 
  filter(!state %in% conus_drop) %>% 
  group_by(state) %>%
  summarise(
    mean_triple_unweighted = mean(triple_vulnerability, na.rm=TRUE),
    mean_triple_weighted   = weighted.mean(triple_vulnerability, enrollment_total, na.rm=TRUE),
    total_students         = sum(enrollment_total, na.rm=TRUE),
    .groups="drop"
  ) %>%
  arrange(desc(mean_triple_weighted)) %>%
  left_join(state_names, by="state") %>%
  relocate(state_name, .before=state) #

save_csv(state_summary, "state_triple_vulnerability_summary") # a nice clean file to show unweighted and weighted means of scores + total students

# Student exposure share
student_exposure <- df_strict %>% # what is the student exposure like? 
  group_by(hotspot) %>% #so basically what is the % of students in hotspots 
  summarise(total_students = sum(enrollment_total, na.rm = TRUE), .groups="drop") %>%
  mutate(share = total_students / sum(total_students))
save_csv(student_exposure, "student_exposure")

#print(nice_tbl(student_exposure, title="Student exposure to hotspots")
      #|#> #save_gt("tbl_student_exposure"))

# Aspirational calculations based on 80th percentile of DPS
aspirational_calc <- function(df, bench_q = cfg$BENCH_Q){ # Going back to the 80th percentile; will talk to EK and PD about this
  bench <- quantile(df$debt_per_student, bench_q, na.rm=TRUE) #going with DPS 
  df %>%
    mutate(target_debt = bench * enrollment_total, #and student enrollment 
           uplift      = pmax(0, target_debt - debt_expanded)) %>%
    summarise(
      current_total      = sum(debt_expanded, na.rm=TRUE), #debt expanded is the $800b instead of the usual $620B school debt market
      aspirational_total = sum(target_debt,   na.rm=TRUE),
      incremental_gap    = sum(uplift,        na.rm=TRUE), 
      pct_growth         = incremental_gap / current_total
    )
}
asp_headline <- aspirational_calc(df_strict)
save_csv(asp_headline, "aspirational_headline")
#print(nice_tbl(asp_headline, title="Aspirational borrowing headline") |> save_gt("tbl_aspirational_headline"))

# =========================================
# 6. How does heat play into all this aspiraitonal stuff? Do hotter districts need to borrow more? Less? 
# ==============================
# 
df_heat <- df_strict %>% mutate(high_heat = d_hsd80_2050_ssp5 >= cfg$HIGH_HEAT_CUTOFF)

bench_dps <- quantile(df_heat$debt_per_student, probs = cfg$BENCH_Q, na.rm = TRUE)
df_realloc <- df_heat %>%
  mutate(target_debt = bench_dps * enrollment_total, uplift= pmax(0, target_debt - debt_expanded), surplus= pmax(0, debt_expanded - target_debt))

realloc_summary <- df_realloc %>%
  group_by(high_heat) %>%
  summarise(
    current_debt = sum(debt_expanded, na.rm=TRUE),
    target_debt  = sum(target_debt,    na.rm=TRUE),
    shortfall    = sum(uplift,         na.rm=TRUE),
    surplus      = sum(surplus,        na.rm=TRUE),
    .groups="drop"
  )
save_csv(realloc_summary, "realloc_summary") # <- this csv shows number of districts facing high heat, what their current debt is
# what their aspirational/target debt should be # what the shortfall is # what the surplus might be and whether its enough to close the gap


# ==============================
# 7) Descriptive stats of the z scores 
# ==============================
cat("Districts with full z-scores:", nrow(df_strict), "\n") # how many districts have all three z scores 
cat("Hotspot threshold (", cfg$HOTSPOT_Q*100, "%): ", round(thr_tv, 3), "\n", sep="") #what is the threshold that hotspots need to be over
cat("Aspirational bench DPS (", cfg$BENCH_Q*100, "%): $", scales::comma(round(bench_dps,2)), "\n", sep="") #what is the aspirational benchmark 

zsum <- function(v) c(summary(v), SD = sd(v, na.rm=TRUE)) #what are the descriptive stats of the z scores 
diag_tbl <- tibble::tibble( #so mean median Q1 Q3 max and then STDEV
  metric = c("z_fiscal","z_social","z_climate","triple_vulnerability"),
  stats  = I(list(zsum(df_strict$z_fiscal),
                  zsum(df_strict$z_social),
                  zsum(df_strict$z_climate),
                  zsum(df_strict$triple_vulnerability))) #helps understand z score distribution 
)
print(diag_tbl)
save_csv(df_strict %>% select(GeoId, z_fiscal, z_social, z_climate, triple_vulnerability, hotspot),
         "diagnostic_scores_min") # this csv will have GEOID, each z score, composite z score and whether it is a hotspot


#=========================
# ANALYSIS ! !
#===========================
# So there are two different definitions of heat that I am not sure which to go with 
#absolute level (≥30 hot days by 2050), or
#delta increase from baseline (≥30 more days)

# So here I am looking at what the results look like for both 

use_filtered <- TRUE          # TRUE = match your Figure 8 filters
heat_def     <- c("absolute","delta")  # run both for clarity
bench_on     <- "filtered"   # "filtered" or "full" (where to take the 80th pct DPS)

BASE <- if (use_filtered) {
  df_strict %>% filter(enrollment_total >= 500, #filtering out districts that are outliers 
                       debt_per_student <= 100000)
} else df_strict

bench_pool <- if (bench_on == "filtered") BASE else df_strict
bench_q    <- 0.80 #80th percentile 
bench_dps  <- quantile(bench_pool$debt_per_student, bench_q, na.rm = TRUE) #DPS benchmark based on the 80th percentile 

#the point here is to calculate the target debt based on DPS x enrollment 
# then calculate the uplift/aspiration 
# and then aggregate this uplift by the heat group (absolute vs delta)
# calcs the number of districts, total students, gap, per student gap, mean of ratio of per student uplift 
summ_by_heat <- function(df, high_heat_flag) { 
  df %>%
    mutate(
      high_heat   = high_heat_flag,
      target_debt = bench_dps * enrollment_total,
      uplift      = pmax(0, target_debt - debt_expanded)
    ) %>%
    group_by(high_heat) %>%
    summarise(
      districts            = n(),
      students             = sum(enrollment_total, na.rm = TRUE),
      total_gap            = sum(uplift, na.rm = TRUE),
      per_student_ratio_of_sums = total_gap / students,                
      mean_ratio                = mean(uplift / enrollment_total, na.rm = TRUE),
      median_ratio              = median(uplift / enrollment_total, na.rm = TRUE),
      per_district_mean         = mean(uplift, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      group                = if_else(high_heat, "High-heat", "Other"),
      total_gap_B          = total_gap / 1e9,
      per_district_mean_M  = per_district_mean / 1e6
    ) %>%
    select(group, districts, students, total_gap_B,
           per_student_ratio_of_sums, mean_ratio, median_ratio,
           per_district_mean_M)
}

# here this is for both definitions of heat 
abs_tbl <- BASE %>% summ_by_heat(high_heat_flag = (.$hsd80_2050_ssp5 >= 30))
del_tbl <- BASE %>% summ_by_heat(high_heat_flag = (.$d_hsd80_2050_ssp5 >= 30))

# outputs 
cat(glue("\nBenchmark DPS (80th pct of {bench_on} sample): ${comma(round(bench_dps))}\n"))
cat(glue("Sample used: {if (use_filtered) 'Filtered (enr≥500, DPS≤$100k)' else 'Full analytic'}\n"))

cat("\n=== ABSOLUTE HEAT (≥30 hot school-year days by 2050, SSP5) ===\n")
print(abs_tbl %>%
        mutate(across(c(per_student_ratio_of_sums, mean_ratio, median_ratio),
                      ~ dollar(.x))))

cat("\n=== DELTA HEAT (increase ≥30 hot school-year days vs baseline, SSP5) ===\n")
print(del_tbl %>%
        mutate(across(c(per_student_ratio_of_sums, mean_ratio, median_ratio),
                      ~ dollar(.x))))


##################################
#=================================
# Why are we looking at 30 hot days? Are we overlooking something by limiting to this?
#================================
# filtered sample as before
df_filtered <- df_strict %>%
  filter(enrollment_total >= 500, # just to filter outliers out 
         debt_per_student <= 100000)

bench_q <- 0.80 #80th PERCENTILE eee
bench_dps <- quantile(df_filtered$debt_per_student, bench_q, na.rm = TRUE) #filter out any NAs

# thresholds to test
thresholds <- c(31:40, "40+") #I'm just testing these we can go beyond this but idk 

# This is a loop that goes over each threshold (31, 32, 33, etc.) and 
# flags each high heat district, what the total is, total number of students, total borrowing gap (how far they are below the benchmark)
# and then the gap per student 
results <- lapply(thresholds, function(thresh) {
  if (thresh == "40+") {
    highheat <- df_filtered$d_hsd80_2050_ssp5 >= 40 #again we can tweak this 
    label <- "≥40"
  } else {
    highheat <- df_filtered$d_hsd80_2050_ssp5 >= as.numeric(thresh)
    label <- paste0("≥", thresh)
  }
  
  D <- df_filtered %>%
    mutate(
      high_heat = highheat,
      target_debt = bench_dps * enrollment_total,
      uplift      = pmax(0, target_debt - debt_expanded)
    )
  
  out <- D %>%
    filter(high_heat) %>%
    summarise(
      threshold     = label,
      districts     = n(),
      students      = sum(enrollment_total, na.rm=TRUE),
      total_gap     = sum(uplift, na.rm=TRUE),
      per_student   = total_gap / students
    )
  
  return(out)
})

results_df <- bind_rows(results)

# Add national stuff to see whether the national aspirational gap is concentrated
# in high heat districts or not 
# interesting either way 
nat_gap <- sum(df_filtered %>%
                 mutate(target_debt = bench_dps * enrollment_total,
                        uplift = pmax(0, target_debt - debt_expanded)) %>%
                 pull(uplift), na.rm=TRUE)

results_df <- results_df %>%
  mutate(
    total_gap_B = total_gap / 1e9,
    per_student = round(per_student, 0),
    share_of_nat = total_gap / nat_gap #this will give us a clear idea of the share 
  )

results_df

