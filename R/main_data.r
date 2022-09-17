# adapted/generalized from Steve Barbeaux' files for
# generating SS files for EBS/AI Greenland Turbot
# ZTA, 2021-10-07, R version 4.05.01 64 bit
### GOA Pacific cod
## Altered in 2022 by Pete Hulson

## write SS3 files for stock assessment

get_pcod_data <- function(afsc_user = NULL,
                          afsc_pass = NULL,
                          akfin_user = NULL,
                          akfin_pass = NULL,
                          old_SS_dat_filename = NULL,
                          new_SS_dat_filename = NULL,
                          new_SS_dat_year = NULL){

## Open up data base connections
AFSC = odbcConnect("AFSC", 
                   afsc_user, 
                   afsc_pass, 
                   believeNRows=FALSE)
CHINA = odbcConnect("AKFIN", 
                    akfin_user, 
                    akfin_pass, 
                    believeNRows=FALSE)

## DEFINE ALL CONSTANTS FOR THIS RUN

# is this a new SS DAT file
is_new_SS_DAT_file <- FALSE

# this assumes that the FUNCTIONS subdirectory is in the working directory
working_dir <- here::here()

# current SS DAT filename
new_SS_dat_filename <- paste0("GOAPcod", 
                              format(Sys.Date(), format = "%Y%b%d"),
                              ".dat")

# the most recent year of data to be used
final_year <- new_SS_dat_year

# the FMP area for this stock
sp_area <- "'GOA'"

# the GOA FMP sub-areas in the COUNCIL.COMPREHENSIVE_BLEND_CA database table
fsh_sp_area <- "'CG','PWSI','SE','SEI','WG','WY'"

# species label for AKFIN
fsh_sp_label <- "'PCOD'"

# the fishery species code(s) for this stock/these stocks
fsh_sp_str <- "202"

# year in which to start the fishery data
fsh_start_yr <- 1977

# fraction of the year that the fishery length- and weight-at-age calculations are done
fsh_frac <- 0.5

# the survey species code(s) for this stock/these stocks
srv_sp_str <- "21720"

# year in which to start the bottom trawl survey data
srv_start_yr <- 1984

# year in which to start the LL survey data
LLsrv_start_yr <- 1990

# fraction of the year that the survey takes place
srv_frac <- 0.5833333333

# length bins to use for fsh and srv length comp data
bin_width <- 1
min_size <- 0.5
max_size <- 116.5  # less than 1% of the fish in each year are 105 cm or larger (max less than 0.6%)
len_bins <- seq(min_size, max_size, bin_width)

# maximum age
max_age <- 10

## Get all the functions for pulling GOA Pcod data
source(here::here("R", "data", "BIN_LEN_DATA.r"))
source(here::here("R", "data", "cond_length_age_corFISH.r"))
source(here::here("R", "data", "conditional_Length_AGE_cor.r"))
source(here::here("R", "data", "find_ALF.r"))
source(here::here("R", "data", "FISH_AGE_COMP.r"))
source(here::here("R", "data", "FORMAT_AGE_MEANS1.r"))
source(here::here("R", "data", "GET_DOM_AGE.r"))
source(here::here("R", "data", "GET_GOA_ACOMP1.r"))
source(here::here("R", "data", "GET_GOA_BIOM.r"))
source(here::here("R", "data", "GET_GOA_LCOMP1.r"))
source(here::here("R", "data", "GET_GOA_LENCOM2.r"))
source(here::here("R", "data", "GET_GOA_LL_RPN.r"))
source(here::here("R", "data", "GET_LENGTH_BY_CATCH_GOA.R"))
source(here::here("R", "data", "GET_SURV_AGE_cor.r"))

## Get all the alternative data that isn't in AKFIN or AFSC databases
OLD_SEAS_GEAR_CATCH <- vroom::vroom(here::here('data', 'OLD_SEAS_GEAR_CATCH.csv'))
Larval_indices <- vroom::vroom(here::here('data', 'Larval_indices.csv'))
ADFG_IPHC <- vroom::vroom(here::here('data', 'ADFG_IPHC.csv'))
ALL_STATE_LENGTHS <- vroom::vroom(here::here('data', 'ALL_STATE_LENGTHS.csv'))
TEMPHW <- vroom::vroom(here::here('data', 'TEMPANDHEAT.csv'))

## Get all data for data file
source(here::here("R", "data", "SBSS_GET_ALL_DATA_GOA_PCOD_cor.r"))

if (!is_new_SS_DAT_file){
  old_data <- SS_readdat_3.30(here::here("data", old_SS_dat_filename))
  new_data <- old_data
}else{print(" Warning:  Need to enter old SS data file name")}

new_data <- SBSS_GET_ALL_DATA(new_data = new_data,
                              new_file = new_SS_dat_filename,
                              new_year = new_SS_dat_year,
                              sp_area = sp_area,
                              fsh_sp_label = fsh_sp_label,
                              fsh_sp_area = fsh_sp_area,
                              fsh_sp_str = fsh_sp_str,
                              fsh_start_yr = fsh_start_yr,
                              srv_sp_str = srv_sp_str,
                              srv_start_yr = srv_start_yr,
                              len_bins = len_bins,
                              max_age = max_age,
                              is_new_SS_DAT_file = is_new_SS_DAT_file,
                              AUXFCOMP = 3)

close(AFSC)
close(CHINA)

new_data
}
