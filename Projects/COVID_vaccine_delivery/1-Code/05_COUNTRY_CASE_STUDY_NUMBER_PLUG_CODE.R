#### #----#                        Docstring                         #----# ####
#' Project:      FGH COVID Vaccine DAH Project 2021: Country Case Studies
#' Purpose:      Number plugging and Summary table generation for Zambia, 
#'               Rwanda, Ghana, and Senegal
#' Description:  Number plugging outputs are not paragraphs and sentences in an 
#'               academic report, but rather bullets in "Case Study" PDF files 
#'               downloadable from a tab within the COVID-19 vaccine delivery 
#'               viz at https://vizhub.healthdata.org/covid-spending/. 
#'               Summary tables are generated as .csv files, but ultimately 
#'               also go into the same .pdf files as the number plugged 
#'               bullets.
#' 
#' Date:         2022-11-22
#' Last Updated: 2022-12-09 (R 4.2.0)
#------------------------------------------------------------------------------#

############################## TEXT TO BE PLUGGED ##############################
#' (NOTE - In each bullet, [COUNTRY] gets replaced with the country of 
#' interest. The request calls for the same text for each country)
#' 
#' `Health system in COUNTRY, paragraph 2`:
#' In 2019, average per person health spending in [COUNTRY] was $XXX (XXX-XXX). 
#' The majority of that spending was from XXX sources (XXX% (XXX-XXX)). As a 
#' share of total health spending, households contributed XXX% (XXX-XXX), 
#' government contributed XXX% (XXX-XXX) and external partners contributed XXX% 
#' (XXX-XXX).
#' 
#' `COVID-19 pandemic experience in COUNTRY, sentence 1`:
#' In aggregate, a cumulative total of XXX persons have died in [COUNTRY] due 
#' to COVID-19 between 2020 and [DATE].
#' 
#' `Conclusions, bullet 1`:
#' Between XXX and XXX, the government spent $XXX (XXX per person) 
#' to support the delivery of COVID-19 vaccine.
#------------------------------------------------------------------------------#

####################### #----# ENVIRONMENT SETUP #----# ########################

# Clean working directory
rm(list=ls())
## Defining j, h, and k 
if (Sys.info()[1] == "Linux"){
  j <- "FILEPATH"
  h <- paste0("FILEPATH", Sys.info()[7])
  k <- "FILEPATH"
} else if (Sys.info()[1] == "Windows"){
  j <- "FILEPATH"
  h <- "FILEPATH"
  k <- "FILEPATH"
}
## Imports
pacman::p_load(testthat)

## Source functions
source(paste0(h, "FILEPATH/utils.R"))
source(paste0(h, "FILEPATH/helper_functions.R"))
# Needed to get country-specific population estimates
source("FILEPATH/get_population.R")

## Local CONSTANTS
FGH_ROUND_ID   <- 14
GBD_RELEASE_ID <- 9
# Date of COVID-19 team outputs to use ("YYYY_MM_DD")
COVID_OUTPUTS_DATE_TAG <- "2022_11_18"
COVID_DRAWS_PATH <- paste0("FILEPATH")
# The year for which to report health expenditure data (default to last year of 
# non-forecasted FGH health spending data)
HE_METRICS_YEAR <- 2019
# The min and max years of interest for which we need population data from the 
# get_population() shared function
MIN_START_YEAR  <- 2020
MAX_END_YEAR    <- 2022
# Case study countries of interest and their ISO3 codes
COUNTRY_ISO_CODES <- c(
  `Zambia`  = "ZMB", # Zambia
  `Rwanda`  = "RWA", # Rwanda
  `Ghana`   = "GHA", # Ghana
  `Senegal` = "SEN"  # Senegal
)
# IHME location IDs for each country of interest 
# (same order as COUNTRY_ISO_CODES list above)
COUNTRY_IHME_LOCATION_IDS <- c(191, 185, 207, 216)
names(COUNTRY_IHME_LOCATION_IDS) <- names(COUNTRY_ISO_CODES)
# GHES for vaccine delivery, 2021-2022 (constants provided from correspondent;
# same order as COUNTRY_ISO_CODES list above)
VACCINE_DELIVERY_GHES <- c(16.7e6, 6573562, 16145094, 701400)
names(VACCINE_DELIVERY_GHES) <- names(COUNTRY_ISO_CODES)
# Dictionary of per person metric full names for print-out
METRIC_FULL_NAMES <- c(
  oop_pc_per_the_pc  = "households",
  ghes_pc_per_the_pc = "the government",
  dah_pc_per_the_pc  = "external partners"
)
# Output path for summary tables
VAX_DELIVERY_PATH <- paste0("FILEPATH FOR OUTPUT SAVE FILE")
# Column names for output summary tables
VAX_DELIVERY_SUMMARY_TABLE_NAMES <- c(
  "Activity",
  "Expenditure (Local)",
  "Expenditure (USD)",
  "Expenditure per person (USD) - Original source",
  "Expenditure per person (USD) - GBD 2021 population estimates"
)
# Report cumulative deaths up to this date (default to last date with
# non-projected cumulative deaths data)
DEATHS_DATE <- "2022-11-10"
#------------------------------------------------------------------------------#


######################## #----# HELPER FUNCTIONS #----# ########################
#' @title create_he_dt
#' @description Calls get_he_data() helper function and returns a data.table 
#' the health expenditure draws for a given metric and year
#' 
#' @param metric [character] Name of the metric to return HE data for (e.g. 
#' "dah_totes", "the_pc")
#' @param year [integer] Year of HE data to return
#' @return Returns a data.table with `metric` column containing draws
create_he_dt <- function (metric, year) {
  dt <- get_he_data(metric, fgh_round_id = FGH_ROUND_ID)[year_id %in% year, ]
  setnames(dt, "data_var", metric)
  
  return (dt)
}

#' @title create_stats
#' @description Gets mean and upper/lower 95% CI bounds for a given draws 
#' variable in a given data.table
#' 
#' @param x [character] Name of draws column
#' @param dt [data.table] Data.table containing the draws var `x`
#' @param id_var [vector] Character vector containing the non-`x` ID 
#' columns for which to include values in output
#' @return Returns a list containing ID vars and stats of `x`
create_stats <- function (x, dt, id_var = c("ihme_loc_id", "year_id")) {
  stats <- dt[, as.list(c(x, mean(get(x)), 
                          quantile(get(x), c(0.025, 0.975)))), 
              by = id_var]
  names(stats) <- c(id_var, "metric", "mean", "lower", "upper")
  
  return(stats)
}

#' @title generate_full_summary_table
#' @description Given the name of a country, a valid HE metric, and a 
#' population estimate, creates and writes out an expenditure "summary table" 
#' with a new calculated per person expenditure column
#' 
#' @param country_name [character] The name of a country of interest for which 
#' a preliminary vax_delivery_summary table exists
#' @param metric_name [character] The name of a valid FGH HE metric for which 
#' a preliminary vax_delivery_summary table exists
#' @param population [integer] Estimated population or average population for 
#' the country of interest
#' @param inputs_date [character] Date tag for preliminary vax_delivery_summary 
#' table filepath
#' @param filename_tag [character] Additional tag for preliminary 
#' vax_delivery_summary table filepath, if applicable
generate_full_summary_table <- function (country_name,
                                         metric_name,
                                         population,
                                         inputs_date = "2022-12-01",
                                         filename_tag = "") {
  # Add underscore to filename_tag if it is specified
  filename_tag <- ifelse(nchar(filename_tag) == 0, filename_tag,
                         paste0("_", filename_tag))
  # Read in summary data with "expenditure_USD" column
  vax_delivery_summary <- fread(paste0(VAX_DELIVERY_PATH))
  # Calculate per person expenditure using supplied population estimate and 
  # existing expenditure column
  vax_delivery_summary[, expenditure_pc_expected := 
                         expenditure_USD / population]
  # Reset column names to standardize across countries using 
  # locally-defined vector
  setnames(
    vax_delivery_summary,
    names(vax_delivery_summary),
    VAX_DELIVERY_SUMMARY_TABLE_NAMES
  )
  # Save new "full" table
  fwrite(
    vax_delivery_summary,
    paste0(VAX_DELIVERY_PATH)
  )
}
#------------------------------------------------------------------------------#


############################# #----# MAIN #----# ###############################
#### #----#                         Data Prep                        #----# ####
## Get THE, OOP, GHES, DAH, all per capita, in HE_METRICS_YEAR
# Define metrics of interest
metrics <- c("the_pc", "oop_pc", "ghes_pc", "dah_pc", "ppp_pc")
# Get HE data for each metric and save each result in list
metrics_dt_list <- lapply(metrics, create_he_dt, year = HE_METRICS_YEAR)
# Merge metrics data.tables on loc, draw, year
metrics_dt <- Reduce(merge, metrics_dt_list)
#------------------------------------------------------------------------------#

#### #----#             Country-specific number plugging             #----# ####
### For each country of interest...
cat(paste0(
  green("PLUGGED SENTENCES FOR THE FOLLOWING COUNTRIES OF INTEREST\n"),
  yellow(paste(toupper(names(COUNTRY_ISO_CODES)), collapse = ", ")),
  green("\n---------------------------------------------------------\n")
))
for (country_name in names(COUNTRY_ISO_CODES)) {
  # For more clean-looking printing
  cat(paste0(
    green("Sentences for "),
    yellow(country_name),
    green("...\n")
  ))
  
  ## Subset metrics to just this one country
  country_metrics <- copy(metrics_dt[
    ihme_loc_id == COUNTRY_ISO_CODES[[country_name]], ])
  
  ## Get per THE draws
  non_the_pc_cols <- metrics[metrics != "the_pc"]
  per_the_pc_cols <- paste0(non_the_pc_cols, "_per_the_pc")
  country_metrics[, (per_the_pc_cols) := lapply(.SD, "/", the_pc), 
                  .SDcols = non_the_pc_cols]
  
  ## Create metrics
  # Note - stats_maker() fails when trying to create stats for more than 3 
  # variables at once, so doing custom operation
  stats_cols <- c("the_pc", per_the_pc_cols)
  stats_list <- lapply(stats_cols,
                       create_stats, dt = country_metrics)
  # To data.table for easier subsetting later
  stats_dt <- rbindlist(stats_list)
  
  ## Convert columns to numeric class for rounding
  numeric_cols <- c("year_id", "mean", "lower", "upper")
  stats_dt[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
  # Metric with the plurality of % of THE
  plurality_metric <- stats_dt[mean == stats_dt[metric != "the_pc", max(mean)],
                               metric]
  # Get "full" name of metric for print-out
  plurality_metric_full <- METRIC_FULL_NAMES[[plurality_metric]]
  #----------------------------------------------------------------------------#
  
  #### #----#         Health system in COUNTRY, paragraph 2          #----# ####
  #' In HE_METRICS_YEAR, average per person health spending in [COUNTRY] was $XXX 
  #' (XXX-XXX). The majority of that spending was from XXX sources 
  #' (XXX% (XXX-XXX)). As a share of total health spending, households 
  #' contributed XXX% (XXX-XXX), government contributed XXX% (XXX-XXX) and 
  #' external partners contributed XXX% (XXX-XXX).
  
  ## Print sentences
  cat(paste0(
    "In ",
    HE_METRICS_YEAR,
    ", average per person health spending in ",
    country_name,
    " was $",
    round_num(stats_dt[metric == "the_pc", mean], "dollar"),
    " (",
    round_num(stats_dt[metric == "the_pc", lower], "dollar"),
    "-",
    round_num(stats_dt[metric == "the_pc", upper], "dollar"),
    ").\n"
  ))
  cat(paste0(
    "The majority of that spending was from ",
    plurality_metric_full,
    " (",
    round_num(stats_dt[metric == plurality_metric, mean], "pct"),
    "% (",
    round_num(stats_dt[metric == plurality_metric, lower], "pct"),
    "-",
    round_num(stats_dt[metric == plurality_metric, upper], "pct"),
    ")).\n"
  ))
  cat(paste0(
    "As a share of total health spending, households contributed ",
    round_num(stats_dt[metric == "oop_pc_per_the_pc", mean], "pct"),
    "% (",
    round_num(stats_dt[metric == "oop_pc_per_the_pc", lower], "pct"),
    "-",
    round_num(stats_dt[metric == "oop_pc_per_the_pc", upper], "pct"),
    "), government contributed ",
    round_num(stats_dt[metric == "ghes_pc_per_the_pc", mean], "pct"),
    "% (",
    round_num(stats_dt[metric == "ghes_pc_per_the_pc", lower], "pct"),
    "-",
    round_num(stats_dt[metric == "ghes_pc_per_the_pc", upper], "pct"),
    ") and external partners contributed ",
    round_num(stats_dt[metric == "dah_pc_per_the_pc", mean], "pct"),
    "% (",
    round_num(stats_dt[metric == "dah_pc_per_the_pc", lower], "pct"),
    "-",
    round_num(stats_dt[metric == "dah_pc_per_the_pc", upper], "pct"),
    ").\n\n"
  ))
  #----------------------------------------------------------------------------#
  
  #### #----#  COVID-19 pandemic experience in COUNTRY, sentence 1   #----# ####
  #' In aggregate, a cumulative total of XXX persons have died in [COUNTRY] due 
  #' to COVID-19 between 2020 and [DATE].
  
  ## Assign country ihme location id to variable (used more than once below)
  country_location_id <- COUNTRY_IHME_LOCATION_IDS[[country_name]]
  
  ## Get COVID team deaths outputs
  # Cumulative deaths for [COUNTRY] on DEATHS_DATE
  # (latest available outputs as of 2022-11-25)
  deaths <- unlist(fread(paste0(
    COVID_DRAWS_PATH, 
    "cumulative_deaths.csv"
  ))[location_id == country_location_id & date == DEATHS_DATE, c(3:102)])
  
  ## Get mean, upper/lower bounds
  deaths_stats <- as.list(c(mean(deaths), quantile(deaths, c(0.025, 0.975))))
  names(deaths_stats) <- c("mean", "lower", "upper")
  
  ## Print sentence
  cat(paste0(
    "In aggregate, a cumulative total of ",
    round_num(deaths_stats$mean, "whole"),
    " (",
    round_num(deaths_stats$lower, "whole"),
    "-",
    round_num(deaths_stats$upper, "whole"),
    ") persons have died in ",
    country_name,
    " due to COVID-19 between 2020 and ",
    DEATHS_DATE,
    ".\n\n"
  ))
  #----------------------------------------------------------------------------#
  
  #### #----#                 Conclusions, bullet 1                  #----# ####
  #' Between XXX and XXX, the government spent $XXX (XXX per person) 
  #' to support the delivery of COVID-19 vaccine.
  
  ## Get GBD pop estimates for this country
  # Note - will be using [COUNTRY] averages or weighted averages for each period
  pops <- get_population(
    age_group_id = 22,                             # all ages
    location_id  = country_location_id,            # [COUNTRY]
    year_id      = c(MIN_START_YEAR:MAX_END_YEAR), # START~END YEARs
    sex_id       = 3,                              # both
    with_ui      = FALSE,                          # return UI bounds?
    release_id   = GBD_RELEASE_ID                  # GBD 2021
  )
  
  ## Conditional logic to handle different reporting periods for different 
  ## countries (Rwanda and Ghana have different reporting periods)
  if (country_name == "Rwanda") {
    # Strings for print-out
    period_start <- "July 2020"
    period_end   <- "June 2022"
    # For Rwanda, we care about 1/2 of 2020, all of 2021, 1/2 of 2022
    pops[year_id == 2021, fraction := 0.5]
    pops[year_id %in% c(2020, 2022), fraction := 0.25]
    # Calculation population for each period
    # Period 1: 2020-07-01~2020-12-31 (6 months), 
    #           2021-01-01~2021-06-31 (6 months)
    period1_pop <- mean(pops[year_id %in% c(2020, 2021), population])
    # Period 2: 2021-07-01~2021-12-31 (6 months), 
    #           2022-01-01~2022-06-31 (6 months)
    period2_pop <- mean(pops[year_id %in% c(2021, 2022), population])
  } else if (country_name == "Ghana") {
    period_start <- "2020"
    period_end   <- "2022"
    # For Ghana, we care about 2020~2022 only, and each 
    # gets 1/3 weight to population
    pops[, fraction := 1/3]
  } else {
    # Strings for print-out
    period_start <- "2021"
    period_end   <- "2022"
    # For other countries, we care about 2021 and 2022 only, and each 
    # gets 50% weight to population
    pops[year_id == 2020, fraction := 0]
    pops[year_id %in% c(2021, 2022), fraction := 0.5]
  }
  
  ## Validation of fraction sums (should sum up to 1)
  expect_equal(pops[, sum(fraction)], 1)
  
  ## Calculate estimated per person vax delivery government spending
  # Calculate population weighted average. Mean, upper, lower pops all appear 
  # to be the same in this case, so just using mean
  population_weighted <- pops[, sum(population * fraction)]
  # Get collaborator-provided vax delivery GHES constant
  country_vax_delivery_ghes <- VACCINE_DELIVERY_GHES[[country_name]]
  # Calculate estimate for per person vax delivery GHES
  ghes_pc <- country_vax_delivery_ghes / population_weighted
  
  ## Determine round_num() `type` based on GHES magnitude
  round_type <- ifelse(
    country_vax_delivery_ghes / 1e9 >= 1,
    "billion",
    ifelse(
      country_vax_delivery_ghes / 1e6 >= 1,
      "million",
      "thousand"
    )
  )
  
  ## Print sentence
  cat(paste0(
    "Between ",
    period_start,
    " and ",
    period_end, 
    " the government spent $",
    round_num(country_vax_delivery_ghes, substr(round_type, 1, 4)),
    " ",
    round_type,
    " ($",
    round_num(ghes_pc, "dollar"),
    " per person) to support the delivery of the COVID-19 vaccine.\n"
  ))
  cat(green("---------------------------------------------------------\n\n"))
  #----------------------------------------------------------------------------#
  
  #### #----#                Summary table generation                #----# ####
  ## Fact check and generate summary table per person GHES values using
  ## GBD 2021 population estimates..
  ## Once again, Rwanda is different. Zambia also has PPP in addition to GHES
  if (country_name %in% c("Senegal", "Ghana", "Zambia")) {
    # Full period summary table
    generate_full_summary_table(
      country_name = country_name,
      metric_name = "ghes",
      population = population_weighted,
      inputs_date = "2022-12-01"
    )
    # If Zambia, we also need to do the PPP table
    if (country_name == "Zambia") {
      generate_full_summary_table(
        country_name = country_name,
        metric_name = "ppp",
        population = population_weighted,
        inputs_date = "2022-12-01"
      )
    }
  } else if (country_name == "Rwanda") {
    # Period 1 summary table
    generate_full_summary_table(
      country_name = country_name,
      metric_name = "ghes",
      population = period1_pop,
      inputs_date = "2022-12-01",
      filename_tag = "period1"
    )
    # Period 2 summary table
    generate_full_summary_table(
      country_name = country_name,
      metric_name = "ghes",
      population = period2_pop,
      inputs_date = "2022-12-01",
      filename_tag = "period2"
    )
  }
  #----------------------------------------------------------------------------#
}
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#