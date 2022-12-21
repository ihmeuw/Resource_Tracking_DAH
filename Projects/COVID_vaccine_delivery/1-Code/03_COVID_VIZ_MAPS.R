#### #----#                    Docstring                    #----# ####
#' Title:    O3_COVID_VIZ_MAPS 
#' Project:  FGH COVID Vaccine DAH Project 2021- Data Maps IDs
#' Purpose : Assigning appropriate aggregate group IDs for the 
#'           funding for Vaccine Delivery DAH as shown in map view
#'     
#' Date: 2022-09-13
#' Last Updated: 
#---------------------------------------------------------------------#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#' NOTE:
#'    The code assigns IDs for sum aggreate values of 4 groups
#'      SOURCE - 95
#'      CHANNEL - 96
#'      HEALTH FOCUS AREA - 97
#'      LOCATION (GBD super region) - 980000 (avoiding Chile Location id)
#'    As this are needed for specific views in the Viz tool
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
INPUT_FILE <- paste0("FILEPATH FOR INPUT O1_COVID_VIZ_DATA_PREP_dataset.csv")
OUTPUT_FILE_PATH <- paste0("FILEPATH FOR OUTPUT O3_COVID_VIZ_MAPS SAVE FILE")


#---------------------------------------------------------------------#

cat("\n\n")
cat(green("\t#######################################\n"),
    green("\t#### BEGIN O2_COVID_VIZ_FLOWS.R #######\n"),
    green("\t#######################################\n\n"))


################### #----# Main #----# ###################

#### ## Read in Data ## ####
cat(paste0(" Read in Data \n"))

# READ in COVID data
dt_input <- fread(INPUT_FILE)

# Dataset has ID 139 in health_focus_area, which is total sum of all Vaccine delivery HFAs
# keep original dataset to append after all subsets are created
dt_1 <- copy(dt_long)
dt_139 <- copy(dt_long)

# splitting
dt_1 <- dt_1[health_focus_area != 139]
dt_139 <- dt_139[health_focus_area == 139]
#--------------------------------------------------------------------------------------#

cat(' All HFA \n\t create 95 - Sources, 96 - Channels, 97 - HFAs, 980000 - Locations subsets \n')
####----# All HFA create 95 96 97 980000 subsets #----# ####

# X X X X- aggregate of all source, channel, HFA, location
dt_95969798 <- dt_1 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(source = 95, channel = 96, health_focus_area = 97, location_id = 980000)] 

# X X X _ - aggregate of all source, channel, HFA
dt_959697 <- dt_1 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'location_id', 'spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(source = 95, channel = 96, health_focus_area = 97)] 

# X X _ X - aggregate of all source, channel, location by HFA
dt_959698 <- dt_1 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'health_focus_area', 'spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(source = 95, channel = 96, location_id = 980000)] 

# X _ X X  - aggregate of all source, HFA, location by a channel
dt_959798 <- dt_1 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'channel','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(source = 95, health_focus_area = 97, location_id = 980000)] 

# _ X X X  - aggregate of all channel, HFA, location by source
dt_969798 <- dt_1%>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'source','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(channel = 96, health_focus_area = 97, location_id = 980000)] 

cat('\n\t\t creating aggregate with 2 ID cols groups')
## creating aggregate with 2 ID cols groups
# X X _ _  - aggregate of all source, channel by HFA location
dt_9596 <- dt_1 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'location_id', 'health_focus_area','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(source = 95, channel = 96)] 

# X _ X _  - aggregate of all source, HFA by a channel location
dt_9597 <- dt_1 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'location_id', 'channel','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(source = 95, health_focus_area = 97)] 

# _ X X _  - aggregate of all channel, HFA by source location
dt_9697 <- dt_1%>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'location_id', 'source','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(channel = 96, health_focus_area = 97)] 

# X _ _ X  - aggregate of all source, location by HFA channel
dt_9598 <- dt_1 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'channel', 'health_focus_area','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(source = 95, location_id = 980000)] 

# _ X _ X  - aggregate of all location, channel by a hfa source
dt_9698 <- dt_1 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'source', 'health_focus_area','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(channel = 96, location_id = 980000)] 

# _ _ X X  - aggregate of all location, HFA by source channel
dt_9798 <- dt_1%>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'source', 'channel','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`( health_focus_area = 97, location_id = 980000)] 

cat('\n\t\t creating aggregate with 3 ID cols groups')
## creating aggregate with 3 ID cols groups
# X _ _ _ - aggregate for source
dt_95 <- dt_1 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'location_id', 'channel', 'health_focus_area','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(source = 95)] 

# _ X _ _ - aggregate for channel
dt_96 <- dt_1 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'location_id','source','health_focus_area', 'spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(channel = 96)] 

# _ _ X _ - aggregate for HFA
dt_97 <- dt_1 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'location_id','source', 'channel', 'spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(health_focus_area = 97)] 

# _ _ _ X  - aggregate for location
dt_98 <- dt_1 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'source', 'channel', 'health_focus_area','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(location_id = 980000)] 
#--------------------------------------------------------------------------------------#

cat(' Total HFA \n\t create 95 - Sources, 96 - Channels, 97 - HFAs, 980000 - Locations subsets \n')
####----# All HFA create 95 96 97 980000 subsets #----# ####
# 95 96 980000 139
# need the option where we still have HFA 139
dt_139_1 <- dt_139 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'health_focus_area','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(source = 95, channel = 96, location_id = 980000)]

# 95 96 _ 139
dt_139_2 <- dt_139 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'location_id', 'health_focus_area','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(source = 95, channel = 96)]

# 95 _ 980000 139
dt_139_3 <- dt_139 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'health_focus_area','channel','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(source = 95, location_id = 980000)]

# _ 96 980000 139
dt_139_4 <- dt_139 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id',  'health_focus_area','source','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(channel = 96, location_id = 980000)]

# 95 _ _ 139
dt_139_5 <- dt_139 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id',  'health_focus_area','channel', 'location_id','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(source = 95)]

# _ 96 _ 139
dt_139_6 <- dt_139 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id',  'health_focus_area','source', 'location_id','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(channel = 96)]

# _ _ 980000 139
dt_139_7 <- dt_139 %>% .[,lapply(.SD, sum, na.rm=T), by=c('year_id', 'health_focus_area', 'channel','source','spending_id'), .SDcols=c('spending_value') ] %>% 
  .[, `:=`(location_id = 980000)]



#---------------------------------------------------------------------#

#### ## Saving Dataset ## ####
cat(' Saving Dataset \n')

# combining datasets to create aggregate dataset
dt_final <- rbind(dt_long[, .(year_id, source, channel, location_id,health_focus_area, spending_id, spending_value)], dt_95969798, dt_959697, dt_959698, dt_959798,
                  dt_969798, dt_9596, dt_9597, dt_9598, dt_9697, dt_9698, dt_9798, dt_95, dt_96, dt_97, dt_98, 
                  dt_139_1, dt_139_2, dt_139_3, dt_139_4, dt_139_5, dt_139_6, dt_139_7)

fwrite(dt_final, file = paste0(OUTPUT_FILE_PATH,'O3_COVID_VIZ_MAPS_dataset.csv'))
# archive file for date run
fwrite(dt_final, file = paste0(OUTPUT_FILE_PATH,'_archive/O3_COVID_VIZ_MAPS_',currentDate,'.csv'))
