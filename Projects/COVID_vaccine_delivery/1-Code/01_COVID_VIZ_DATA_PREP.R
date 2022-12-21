#### #----#                    Docstring                    #----# ####
#' Title:    O1_COVID_VIZ_DATA_PREP 
#' Project:  FGH COVID Vaccine DAH Project 2021- Data Prep
#' Purpose : Data prep for IHME FGH COVID Vaccine DAH project for Viz tool
#'           Data from IHME FGH COVID DAH dataset is gathered and prep for
#'           appropriate split in followup code. 
#'     
#' Date: 2022-09-13
#' Last Updated: 
#---------------------------------------------------------------------#

################### #----# ENVIRONMENT SETUP #----# ###################
cat(paste0(" Environment Setup \n"))

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
## Source functions
source(paste0(h, "FILEPATH/utils.R"))
source(paste0(h, "FILEPATH/helper_functions.R"))
#----# Local CONSTANTS #----#
currentDate <- format(Sys.time(), "%Y%m%d")

# FILEPATHS
# file specific for regional data needed for the plot
INPUT_FILE <- paste0("FILEPATH FOR INPUT FGH COVID DATA FILE")
OUTPUT_FILE_PATH <- paste0("FILEPATH FOR OUTPUT SAVE FILE")
LOCATIONS_FILEPATH <- paste0("FILEPATH FOR IHME LOCATIONS DATA FILE")
FLOW_ID_FILEPATH <- paste0("FILEPATH FOR IDs DATAFILE")

# lists
# labels for Covid Vaccine Delivery program areas and health focus area labels' Flow IDs
hfa_fids <- list(
  VAX_MOBIL = 130,    # Demand creation and social mobilization
  VAX_SC = 131,       # Supply chain
  VAX_WASTE = 132,    # Waste management
  VAX_HR = 133,       # Human resources
  VAX_TA = 134,       # Technical assistance
  VAX_COORD = 135,    # Planning and coordination
  VAX_DELIVERY = 136, # Disbursement and Delivery
  VAX_SAFETY = 137,   # Safety
  `VAX_M&E` = 138)      # Monitoring

deliv_cols <- names(hfa_fids)
# important bilaterals to call out, DAC and China
bilat_locs <- c('USA', 'GBR', 'DEU', 'FRA', 'CAN', 'AUS', 
                'JPN', 'NOR', 'ESP', 'NLD', 'AUT', 'BEL', 
                'DNK', 'FIN', 'GRC', 'IRL', 'ITA', 'KOR', 
                'LUX', 'NZL', 'PRT', 'SWE','CHE', 'CHN')

#---------------------------------------------------------------------#

cat("\n\n")
cat(green("\t###########################################\n"),
    green("\t#### BEGIN O1_COVID_VIZ_DATA_PREP.R #######\n"),
    green("\t###########################################\n\n"))

################### #----# Main #----# ###################

#### ## Read in Data ## ####
cat(paste0(" Read in Data \n"))

# READ in COVID data
dt <-fread(INPUT_FILE) 
dt <- dt[ELIM_CH==0 & ELIM_DONOR ==0]

# Read in IDs
flow_id <- fread(FLOW_ID_FILEPATH)

# Get ISOcodes and country names
locations_all <- fread(LOCATIONS_FILEPATH)

#---------------------------------------------------------------------#

#### ## Format Data ## ####
cat(paste0(" Format Data \n"))
## Group sources
dt[REPORTING_AGENCY=="UNFPA" & INCOME_TYPE == "CENTRAL", INCOME_SECTOR := "PUBLIC"]
dt[DONOR_NAME == 'BMGF' | CHANNEL == 'BMGF' | INCOME_SECTOR == "BMGF", SOURCE := "BMGF"]
dt[INCOME_SECTOR == "INK", SOURCE := "PRIVINK"]
dt[INCOME_SECTOR == 'UNSP', SOURCE := 'UNALL']

## labeling important Bilaterals as a Source
for (loc in bilat_locs) {
  dt[INCOME_SECTOR == "PUBLIC" & ISO_CODE == loc, SOURCE := loc]
}

## List of 24 DAC (Development assistance committee) countries that we keep track of 
dt[INCOME_SECTOR == "PUBLIC" & ISO_CODE %in% c("CZE", "HUN", "ISL", "POL", "SVK", "SVN"), 
   SOURCE := "OTHERDAC"] 

## Non-OECD DAC countries including ARE
dt[INCOME_SECTOR == "PUBLIC" & (SOURCE == "" | is.na(SOURCE)), SOURCE := "OTHERPUB"] 

## Assign SOURCE as Income sector
dt[SOURCE == "" | is.na(SOURCE), SOURCE := INCOME_SECTOR]
dt[SOURCE == "MULTI", SOURCE := "OTHER"]
# allocating BMGF as private source and US foundarion agency
dt[SOURCE == "BMGF", SOURCE := "PRIVATE"]
dt[CHANNEL == "BMGF", CHANNEL := "US_FOUND"]

## Get label for recipient region
setnames(dt, 'ISO3_RC', 'ihme_loc_id')
dt <- merge(dt, locations_all[,.(ihme_loc_id, super_region_name)], by = 'ihme_loc_id')
# fixing some region labels
dt[ihme_loc_id == "G", super_region_name := "Global"]
dt[ihme_loc_id %in% c("QZA", "S2","S3", "S5", "S6"), super_region_name := "Unallocable"]
dt[ihme_loc_id == "ARG", super_region_name := "Latin America and Caribbean"] # fix Argentina

#---------------------------------------------------------------------#

#### ## Calculate Vaccine Delivery Proportions ## ####
cat(paste0(" Format Data \n"))

## getting Vaccine Delivery specific columns
vax_cols <- names(dt)[names(dt) %like% 'VAX_']

# calculating the total for Vaccine Delivery
dt[, vacc_tot := rowSums(.SD, na.rm = T), .SDcols=vax_cols]
dt[, is_vacc := ifelse(vacc_tot > 0, 1, 0)]

# get proportions for vax delivery
dt[, eval(paste0(deliv_cols, "_prop")) := lapply(.SD, function(x) ifelse(TOTAL_AMT != 0, x/TOTAL_AMT, 0)), .SDcols=paste0(deliv_cols,"_AMT")]
# calculate Disbursement and Commitment for Vax Delivery
dt[, eval(paste0(deliv_cols, '_DISB')) := lapply(.SD, function(x) x * DISBURSEMENT), .SDcols=paste0(deliv_cols,"_prop")]
dt[, eval(paste0(deliv_cols, '_COMM')) := lapply(.SD, function(x) x * COMMITMENT), .SDcols=paste0(deliv_cols,"_prop")]

dt[, DELIVERY_DISB := rowSums(.SD, na.rm = T), .SDcols=paste0(deliv_cols,"_DISB")]
dt[, DELIVERY_COMM := rowSums(.SD, na.rm = T), .SDcols=paste0(deliv_cols,"_COMM")]
dt[, DELIVERY_AMT := rowSums(.SD, na.rm = T), .SDcols=paste0(deliv_cols,"_AMT")]

# fixing recipient names and adding location ids
dt[ihme_loc_id == 'QZA' & RECIPIENT_COUNTRY %like% "VENEZ", `:=` (ihme_loc_id = 'VEN')] 
dt[ihme_loc_id == 'QZA' & RECIPIENT_COUNTRY %like% "WEST BANK AND GAZA ST", `:=` (ihme_loc_id = 'PSE', RECIPIENT_COUNTRY = 'PALESTINE')] 
dt[ihme_loc_id == 'QZA' & RECIPIENT_COUNTRY != "UNALLOCABLE", `:=` (ihme_loc_id = 'QZA',RECIPIENT_COUNTRY = "UNALLOCABLE")] 


dt_wlocs <- merge(dt, locations[, .(ihme_loc_id = local_id, location_name, location_id)], by = 'ihme_loc_id', all.x = T)
dt_wlocs[, location_name := ifelse(is.na(location_name), RECIPIENT_COUNTRY, location_name)]


## aggregate sum by groups 
group_cols <- c('YEAR', 'SOURCE', 'CHANNEL', 'super_region_name','location_name', 'location_id', 'is_vacc')
calc_cols = c(paste0(deliv_cols,"_AMT"), 'DELIVERY_DISB','DELIVERY_COMM','DELIVERY_AMT',paste0(deliv_cols,"_COMM"),paste0(deliv_cols,"_DISB"))
dt_agg_sum <- dt_wlocs[, lapply(.SD, sum, na.rm=T), by = group_cols, .SDcols = calc_cols]

dt_long <- melt.data.table(dt_agg_sum, id.vars = c('YEAR', 'SOURCE', 'CHANNEL', 'super_region_name','location_name', 'location_id'),
                           measure.vars = c(paste0(deliv_cols,"_AMT"),'DELIVERY_AMT',paste0(deliv_cols,"_COMM"),'DELIVERY_COMM',
                                            paste0(deliv_cols,"_DISB"), 'DELIVERY_DISB'))

#---------------------------------------------------------------------#

#### ## Assigning labels and IDs to columns ## ####
cat(' Assigning labels and IDs to columns \n')

# create labels
setnames(dt_long, old = names(dt_long), new=c('year_id', 'source_name', 'channel_name', "region_name", 
                                              "location_name","location_id", 'health_focus_area_name', 'spending_value'))

# assigning specifc IDs for Vaccine Develiry project areas
for (hfa_val in names(hfa_fids)) {
  dt_long[health_focus_area_name %like% paste0(hfa_val,'_'),`:=` (health_focus_area = hfa_fids[hfa_val][[1]])]
}

# assign 139 as value for all Delivery label
dt_long[health_focus_area_name %like% '^DELIVERY_', `:=` (health_focus_area = 139)]

# setting health spending IDs
dt_long[health_focus_area_name %like% '_AMT', `:=` (spending_id = 1)]
dt_long[health_focus_area_name %like% '_COMM', `:=` (spending_id = 2)]
dt_long[health_focus_area_name %like% '_DISB', `:=` (spending_id = 3)]



# merging with the flow_id dataset to get id codes for each column
# note: location_id already exists
dt_long <- merge(dt_long, flow_id[,.(entity_id, entity_short)], by.x = 'source_name', by.y = 'entity_short', all.x = T)
setnames(dt_long, 'entity_id', 'source')
dt_long <- merge(dt_long, flow_id[,.(entity_id, entity_short)], by.x = 'channel_name', by.y = 'entity_short', all.x = T)
setnames(dt_long, 'entity_id', 'channel')
dt_long <- merge(dt_long, flow_id[,.(entity_id, entity_short)], by.x = 'region_name', by.y = 'entity_short', all.x = T)
setnames(dt_long, 'entity_id', 'region_id')

dt_final <- copy(dt_long[spending_value > 0])

#---------------------------------------------------------------------#

#### ## Saving Dataset ## ####
cat(' Saving Dataset \n')

fwrite(dt_final, file = paste0(OUTPUT_FILE_PATH,'O1_COVID_VIZ_DATA_PREP_dataset.csv'))
# archive file for date run
fwrite(dt_final, file = paste0(OUTPUT_FILE_PATH,'_archive/O1_COVID_VIZ_DATA_PREP_',currentDate,'.csv'))
