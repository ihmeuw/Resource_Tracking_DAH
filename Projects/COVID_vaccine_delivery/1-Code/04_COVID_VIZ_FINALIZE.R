#### #----#                    Docstring                    #----# ####
#' Title:    O4_COVID_VIZ_FINALIZE 
#' Project:  FGH COVID Vaccine DAH Project 2021- Finalize data compile
#' Purpose : Assigns the last aggregate IDs for groups:
#'            sources - other governments source, other sources, 
#'                      private philanthropy
#'            channels - development banks, other bilateral aid agencies,
#'                      UN agencies
#'     
#' Date: 2022-09-13
#' Last Updated: 
#---------------------------------------------------------------------#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#' NOTE:
#'       The ID entity names in the lists can be found in the 
#'       vaccine_flows_ids file. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

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
INPUT_FLOW_FILE <- paste0("FILEPATH FOR INPUT O2_COVID_VIZ_FLOWS_dataset.csv")
INPUT_MAPS_FILE <- paste0("FILEPATH FOR INPUT O3_COVID_VIZ_MAPS_dataset.csv")
OUTPUT_FILE_PATH <- paste0("FILEPATH FOR OUTPUT O4_COVID_VIZ_FINALIZE FLOWS and MAPS SAVE FILES")

# lists
other_governments_source <- c(4,5,13,19,25,34,35,37,39,54,55,57,59,70,73,76,77,117,118)
other_sources <- c(12, 99, 82)
private_philanthropy <- c(11, 66,6)
development_banks <- c(1, 2, 33, 93, 94)
other_bilateral_aid <- c(4, 5, 13, 19, 25, 34, 35, 37, 39, 54, 55, 59, 70, 73, 76, 77, 114, 117)
un_agencies <- c(68, 81, 83, 84, 113, 92)
#---------------------------------------------------------------------#

cat("\n\n")
cat(green("\t#######################################\n"),
    green("\t#### BEGIN O2_COVID_VIZ_FLOWS.R #######\n"),
    green("\t#######################################\n\n"))


################### #----# Main #----# ###################

#### ## Read in Data ## ####
cat(paste0(" Read in Data \n"))

# READ in COVID data
dt_in_flow <- fread(INPUT_FLOW_FILE)
dt_in_maps <- fread(INPUT_MAPS_FILE)

#---------------------------------------------------------------------#
cat(green(' Create IDs for aggregate categories datasets '))
#####----# Create IDs for aggregate categories datasets  #----# ####

## creating function to assign labels
assign_group_label <- function(dataset) {
  #' @title               assign_group_label
  #' @description         Assigns the appropriate aggregate label for group
  #'                      
  #' @param dataset       datatable object with the data to assign label              
  #' 
  #'                       
  #' @return dataset with updated group labels
  
  dt_final <- copy(dataset)
  
  # group columns to aggregate by
  group_cols <- c('year_id', 'source', 'channel', 'location_id', 'health_focus_area', 'spending_id')
  
  # id 62, other governments source - sum of multiple donor countries
  id_62 <- copy(dt_final)
  id_62 <- id_62[source %in% other_governments_source]
  id_62[, source := 62]
  id_62 <- id_62[, lapply(.SD, sum, na.rm=T), by = group_cols, .SDcols = c('spending_value')]
  
  # id 67 other sources source - sum of debt repayments, other sources, unallocable
  id_67 <- copy(dt_final)
  id_67 <- id_67[source %in% other_sources]
  id_67[, source := 67]
  id_67 <- id_67[, lapply(.SD, sum, na.rm=T), by = group_cols, .SDcols = c('spending_value')]
  
  # id 71, private philanthropy source. sum of corporate donations and other private
  id_71 <- copy(dt_final)
  id_71 <- id_71[source %in% private_philanthropy]
  id_71[, source := 71]
  id_71 <- id_71[, lapply(.SD, sum, na.rm=T), by = group_cols, .SDcols = c('spending_value')]
  
  # id 14, development banks channel. sum of AfDB, AsDB, IDB, WB_IBRD, WB_IDA
  id_14 <- copy(dt_final)
  id_14 <- id_14[channel %in% development_banks]
  id_14[, channel := 14]
  id_14 <- id_14[, lapply(.SD, sum, na.rm=T), by = group_cols, .SDcols = c('spending_value')]
  
  # id 61, other bilateral aid agencies channel. sum of multiple bilaterals
  id_61 <- copy(dt_final)
  id_61 <- id_61[channel %in% other_bilateral_aid] # removed 36
  id_61[, channel := 61]
  id_61 <- id_61[, lapply(.SD, sum, na.rm=T), by = group_cols, .SDcols = c('spending_value')]
  
  # id 80, UN agencies channel. sum of PAHO, UNAIDS, UNFPA, UNICEF, UNITAID, WHO
  id_80 <- copy(dt_final)
  id_80 <- id_80[channel %in% un_agencies]
  id_80[, channel := 80]
  id_80 <- id_80[, lapply(.SD, sum, na.rm=T), by = group_cols, .SDcols = c('spending_value')]
  
  dt_final <- rbind(dt_final, id_62, id_67, id_71, id_14, id_61, id_80)
  
  # other governments source to dev banks, other bilateral aid agencies, and UN agencies channels
  id_62[channel %in% development_banks, 
        channel := 14]
  id_62[channel %in% other_governments_source, 
        channel := 61]
  id_62[channel %in% un_agencies, 
        channel := 80]
  id_62 <- id_62[channel %in% c(14, 61, 80)]
  id_62 <- id_62[, lapply(.SD, sum, na.rm=T), by = group_cols, .SDcols = c('spending_value')]
  
  # other sources source to dev banks and UN agencies channels
  id_67[channel %in% development_banks, 
        channel := 14]
  id_67[channel %in% un_agencies, 
        channel := 80]
  id_67 <- id_67[channel %in% c(14, 80)]
  id_67 <- id_67[, lapply(.SD, sum, na.rm=T), by = group_cols, .SDcols = c('spending_value')]
  
  # private philanthropy source to UN agencies channel
  id_71[channel %in% un_agencies, 
        channel := 80]
  id_71 <- id_71[channel == 80]
  id_71 <- id_71[, lapply(.SD, sum, na.rm=T), by = group_cols, .SDcols = c('spending_value')]
  
  dt_final <- rbind(dt_final, id_62, id_67, id_71)
  
  return(dt_final)
}


# assigning labels by passing datasets into function
dt_flow <- assign_group_label(dt_in_flow)
dt_maps <- assign_group_label(dt_in_maps)

## final prep for maps dataset
# for dt_maps dropping values that have location 980000 since it is not needed for the map view, location ID is not a country
dt_maps <- dt_maps[location_id != 980000]
dt_maps <- dt_maps[,.(spending_id,year_id,source,channel,location_id,health_focus_area, value = spending_value)]

# removing UNALLOCABLE from dataset since does not show up on map
dt_maps <- dt_maps[!is.na(location_id)]

#--------------------------------------------------------------------------------------#

#### ## Saving Dataset ## ####
cat(' Saving Dataset \n')

fwrite(dt_flow, file = paste0(OUTPUT_FILE_PATH,'covid_vaccine_delivery_flows.csv'))
fwrite(dt_maps, file = paste0(OUTPUT_FILE_PATH,'covid_map.csv'))
# archive file for date run
fwrite(dt_flow, file = paste0(OUTPUT_FILE_PATH,'_archive/covid_vaccine_delivery_flows_',currentDate,'.csv'))
fwrite(dt_maps, file = paste0(OUTPUT_FILE_PATH,'_archive/covid_map_',currentDate,'.csv'))

