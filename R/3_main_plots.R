## Script to run 2022 GOA Pacific Cod Assessment plot code

#######################################################################################
######## Load required packages & define parameters

libs <- c("r4ss",
          "KernSmooth",
          "stringr",
          "data.table",
          "ggplot2",
          "RODBC",
          "data.table",
          "ggplot2",
          "car",
          "dplyr",
          "tidyr",
          "magrittr")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}

lapply(libs, library, character.only = TRUE)

# Current model name
Model_name <- "Model19.1 (22)"

# Current assessment year
new_SS_dat_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# Do you want to call data? If so, set up connections
data_query = TRUE

if(data_query == TRUE){
  db <- read.csv(here::here("database_specs.csv"))
  afsc_user = db$username[db$database == "AFSC"]
  afsc_pass = db$password[db$database == "AFSC"]
  akfin_user = db$username[db$database == "AKFIN"]
  akfin_pass = db$password[db$database == "AKFIN"]
  
  AFSC = odbcConnect("AFSC", 
                     afsc_user, 
                     afsc_pass, 
                     believeNRows=FALSE)
  CHINA = odbcConnect("AKFIN", 
                      akfin_user, 
                      akfin_pass, 
                      believeNRows=FALSE)}

#######################################################################################
######## Plot base model

load(here::here("output", "model_run.RData"))
r4ss::SS_plots(model_run,
               printfolder = "",
               dir = here::here("plots", "assessment"))


#######################################################################################
######## Plot retrospective analysis

load(here::here("output", "retroSummary.RData"))

# make plots comparing the retrospective models
endyrvec <- retroSummary[["endyrs"]] + 0:-10

# r4ss::SSplotComparisons(retroSummary,
#                         endyrvec = endyrvec,
#                         legendlabels = paste("Data", 0:-10, "years"),
#                         print = TRUE,
#                         plotdir = here::here("plots", "retro"))

rho_output_ss3diags <- ss3diags::SSplotRetro(retroSummary,
                                             subplots = c("SSB"),
                                             endyrvec = endyrvec,
                                             legendlabels = paste("Data", 0:-10, "years"),
                                             print = TRUE,
                                             plotdir = here::here("plots", "nonSS"),
                                             pwidth = 8.5,
                                             pheight = 4.5)

#######################################################################################
######## Plot fancy phase-plane

load(here::here("output", "mgmnt_scen.RData"))
load(here::here("output", "model_run.RData"))
source(here::here("R", "plots", "phase_plane_figure.r"))

Fabc = mscen$Tables$F$scenario_1[16]
Fmsy = mscen$Tables$F$scenario_7[16]
SSB0 = mscen$Two_year$SB100[1]
SSBproj1 = mscen$Two_year$SSB[1]
SSBproj2 = mscen$Two_year$SSB[2]
Fproj1 = mscen$Two_year$F40[1]
Fproj2 = mscen$Two_year$F40[2]
BoverBmsy = model_run$timeseries$SpawnBio[3:((new_SS_dat_year - 1977) + 5)] / 2 / (SSB0 * 0.35)  ## SSB w/ 2-year projection
FoverFmsy = model_run$sprseries$F_report[1:((new_SS_dat_year - 1977) + 3)] / Fmsy  

plot.phase.plane(SSB0 = SSB0,
                 Fabc = Fabc,
                 Fmsy = Fmsy,
                 BoverBmsy = BoverBmsy, 
                 FoverFmsy = FoverFmsy,
                 xlim = c(0, 5),
                 ylim = c(0, 1.5),
                 header = "Pacific cod 2022 Model 19.1",
                 eyr = new_SS_dat_year + 2)

dev.print(png, file = here::here("plots", "nonSS", "phase_plane.png"), width = 700, height = 700)
dev.off()


#######################################################################################
######## Plot MCMC

load(here::here("output", "mcmc.RData"))
source(here::here("R", "plots", "mcmcplots.r"))

mcmc_dir <- here::here("Stock_Synthesis_files", Model_name, "MCMC")

mcmc_plots <- plot_mcmc(mcmc_dir, new_SS_dat_year)

# Save plot
multiplot(mcmc_plots[[1]], mcmc_plots[[2]], cols = 1)
dev.print(png, file = here::here("plots", "nonSS", "SSB_Rec.png"), width = 1024, height = 1000)
dev.off()

#######################################################################################
######## Plot Cumulative catch

source(here::here("R", "plots", "cumulative_catch_plots.r"))

# Current week
curr_wk <- as.numeric(format(Sys.Date(), format = "%W"))

# Get cumulative catch plots
cumul_plots <- plot_cumulative(data_query = TRUE,
                               species = "'PCOD'",
                               FMP_AREA = "'GOA'",
                               syear = new_SS_dat_year - 5,
                               CYR = new_SS_dat_year,
                               curr_wk = curr_wk)

cumul_plots[[1]]
dev.print(png, file = here::here("plots", "nonSS", "cummC_CG.png"), width = 700, height = 400)
dev.off()

cumul_plots[[2]]
dev.print(png, file = here::here("plots", "nonSS", "cummC_WG.png"), width = 700, height = 400)
dev.off()


#######################################################################################
######## Plot fishery condition & number of vessels

source(here::here("R", "plots", "Fisheries_Condition.r"))

# Fish condition
cond_plot <- plot_fish_cond(CYR = new_SS_dat_year,
                            data_query = FALSE)

cond_plot[[1]]
dev.print(png, file = here::here("plots", "nonSS", "Cond_WGOA.png"), width = 700, height = 700)
dev.off()

cond_plot[[2]]
dev.print(png, file = here::here("plots", "nonSS", "Cond_CGOA.png"), width = 700, height = 700)
dev.off()

## number of vessels
num_vess <- num_fish_vess(CYR = new_SS_dat_year,
                          data_query = FALSE)

num_vess
dev.print(png, file = here::here("plots", "nonSS", "num_vess.png"), width = 700, height = 400)
dev.off()


#######################################################################################
######## Plot PCod bycatch in pollock and swf fisheries

source(here::here("R", "plots", "cod_bycatch_plots.r"))

# Pollock plots
pol_plots <- pollock_bycatch(data_query = FALSE)

multiplot(pol_plots[[1]], pol_plots[[2]], cols = 1)
dev.print(png, file = here::here("plots", "nonSS", "poll_bycatch.png"), width = 700, height = 700)
dev.off()

# SWF plots
swf_plot <- swf_bycatch(CYR = new_SS_dat_year, data_query = FALSE)

swf_plot
dev.print(png, file = here::here("plots", "nonSS", "swf_bycatch.png"), width = 700, height = 400)
dev.off()

#######################################################################################
######## Plot index time series

source(here::here("R", "plots", "index_figures.r"))

# Plot indices
index_plots <- plot_indices(styr = 1990, endyr = new_SS_dat_year)

index_plots[[1]]
dev.print(png, file = here::here("plots", "nonSS", "fitted_indices.png"), width = 700, height = 700)
dev.off()

index_plots[[2]]
dev.print(png, file = here::here("plots", "nonSS", "nonfitted_indices.png"), width = 700, height = 700)
dev.off()

index_plots[[3]]
dev.print(png, file = here::here("plots", "nonSS", "age0_index.png"), width = 700, height = 400)
dev.off()


#######################################################################################
######## Plot Leave-One-Out analysis results

load(here::here("output", "LOO.RData"))

# Plot parameters from Leave one out
LOO[[2]]
dev.print(png, file = here::here("plots", "nonSS", "LOO.png"), width = 700, height = 700)
dev.off()

