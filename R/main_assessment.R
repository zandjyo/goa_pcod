## Script to run 2022 GOA Pacific Cod Assessment

#######################################################################################
######## Load required packages

libs <- c("data.table",
          "devtools",
          "dplyr",
          "FSA",
          "ggplot2",
          "here",
          "lubridate",
          "mgcv",
          "nlstools",
          "r4ss",
          "R.utils",
          "reshape2",
          "rgdal",
          "RODBC",
          "sizeMat",
          "ss3diags",
          "tidyverse",
          "vroom")

lapply(libs, library, character.only = TRUE)

#######################################################################################
######## Run Data script

# Source data function
source(here::here("R", "main_data.R"))

# Define data fcn parameters
afsc_user = "hulsonp"   ## enter afsc username
afsc_pass = "Liam1Fin2Bri3<3!" ## enter afsc password
akfin_user = "phulson"  ## enter AKFIN username
akfin_pass = "$blwins1" ## enter AKFIN password

# previous SS DAT filename, if it exists
old_SS_dat_filename <- "GOAPcod2021OCT1_10P_CL.dat"

# current SS DAT filename
new_SS_dat_filename <- paste0("GOAPcod", 
                              format(Sys.Date(), format = "%Y%b%d"),
                              ".dat")

# Current assessment year
new_SS_dat_year <- 2022

# Run data script
assessment_data <- get_pcod_data(afsc_user,
                                 afsc_pass,
                                 akfin_user,
                                 akfin_pass,
                                 old_SS_dat_filename,
                                 new_SS_dat_filename,
                                 new_SS_dat_year)

# Write out data script
r4ss::SS_writedat_3.30(assessment_data,
                       here::here("output", new_SS_dat_filename))

# test that the new file is readable
test_dat <- r4ss::SS_readdat_3.30(here::here("output", new_SS_dat_filename), verbose = TRUE)

#######################################################################################
######## Run base model

model_dir <- here::here("Stock_Synthesis_files", "Model19.1 (22)")

r4ss::run(dir = model_dir, 
          skipfinished = FALSE,
          show_in_console = TRUE)


# read the model output and print diagnostic messages
model_run <- r4ss::SS_output(dir = model_dir,
                    verbose = TRUE,
                    printstats = TRUE)

# plot the results
r4ss::SS_plots(model_run)


#######################################################################################
######## Run retrospective analysis

r4ss::retro(dir = model_dir,
      oldsubdir = "",
      newsubdir = "retrospectives",
      years = 0:-10)

# load the retrospective models
retroModels <- r4ss::SSgetoutput(dirvec = file.path(
  model_dir, "retrospectives",
  paste("retro", 0:-10, sep = "")))

# summarize the model results
retroSummary <- r4ss::SSsummarize(retroModels)

# create a vector of the ending year of the retrospectives
endyrvec <- retroSummary[["endyrs"]] + 0:-10

# make plots comparing the retrospective models
r4ss::SSplotComparisons(retroSummary,
                        endyrvec = endyrvec,
                        #legend = FALSE,
                        legendlabels = paste("Data", 0:-10, "years"),
                        print = TRUE,
                        plotdir = here::here("Stock_Synthesis_files", "Model19.1 (22)", "retrospectives", "retro-plots"))

ss3diags::SSplotRetro(retroSummary,
                      subplots = c("SSB"),
                      endyrvec = endyrvec,
                      #legendlabels = paste("Data", 0:-10, "years"),
                      print = TRUE,
                      plotdir = here::here("Stock_Synthesis_files", "Model19.1 (22)", "retrospectives", "retro-plots"))

# calculate Mohn's rho
rho_output <- r4ss::SSmohnsrho(summaryoutput = retroSummary,
                               endyrvec = endyrvec,
                               startyr = retroSummary[["endyrs"]] - 10,
                               verbose = FALSE)

# Save output
save(retroSummary, file = here::here("output", "retroSummary.RData"))
write.csv(retroSummary$SpawnBio, here::here("output", "retro_SSB.csv"))
write.csv(retroSummary$recruits, here::here("output", "retro_Rec.csv"))
write.csv(rho_output, here::here("output", "retro_Rho.csv"))


#######################################################################################
######## Run management scenarios

source(here::here("R", "assessment", "run_mngmnt_scenarios.r"))

# Run management scenarios function
mscen <- Do_AK_Scenarios(Model_name = "Model19.1 (22)",
                         CYR = year(Sys.Date()),
                         SYR = 1977,
                         FCASTY = 15,
                         FLEETS = c(1:3),
                         do_fig = TRUE)

# Save output
save(mscen, file = here::here("output", "mngmnt_scen.RData"))
write.csv(mscen$Tables, here::here("output", "mgmnt_scen_table.csv"))
write.csv(mscen$Two_year, here::here("output", "exec_summ.csv"))

# ggsave()



#######################################################################################
######## Run Leave-One-Out Analysis

source(here::here("R", "assessment", "LeaveOneOut.r"))

LOO <- SS_doLOO(Model_name = "Model19.1 (22)",
                newsubdir = "LeaveOneOut", 
                years = 0:-10,
                datafilename = new_SS_dat_filename,
                CYR = year(Sys.Date()))

# Save output
save(LOO, file = here::here("output", "LOO.RData"))
write.csv(LOO$Tables, here::here("output", "LOO_table.csv"))



#######################################################################################
######## Run Jitter Test

# define a new directory
if (!file.exists(here::here("Stock_Synthesis_files", Model_name, "jitter"))) 
  dir.create(here::here("Stock_Synthesis_files", Model_name, "jitter"))
jitter_dir <- here::here("Stock_Synthesis_files", "Model19.1 (22)", "jitter")

# copy over the stock synthesis model files to the new directory
r4ss::copy_SS_inputs(dir.old = here::here("Stock_Synthesis_files", Model_name), 
                     dir.new = here::here("Stock_Synthesis_files", Model_name, "jitter"),
                     overwrite = TRUE)
base::file.copy(from = here::here("Stock_Synthesis_files", Model_name, "ss.exe"),
                to = here::here("Stock_Synthesis_files", Model_name, "jitter", "ss.exe"),
                overwrite = TRUE)
base::file.copy(from = here::here("Stock_Synthesis_files", Model_name, "ss.par"),
                to = here::here("Stock_Synthesis_files", Model_name, "jitter", "ss.par"),
                overwrite = TRUE)

# run the jitters
jitter_loglike <- jitter(dir = jitter_dir,
                         Njitter = 50,
                         jitter_fraction = 0.05)

# Save output
write.csv(jitter_loglike, here::here("output", "jitter_table.csv"))



#######################################################################################
######## Run MCMC


# Write SS files in MCMC subfolder
r4ss::copy_SS_inputs(dir.old = model_dir, 
                     dir.new = here::here("Stock_Synthesis_files", "Model19.1 (22)", "MCMC"),
                     overwrite = TRUE)
base::file.copy(from = here::here("Stock_Synthesis_files", "Model19.1 (22)", "ss.exe"),
                to = here::here("Stock_Synthesis_files", "Model19.1 (22)", "MCMC", "ss.exe"),
                overwrite = TRUE)
base::file.copy(from = here::here("Stock_Synthesis_files", "Model19.1 (22)", "ss.par"),
                to = here::here("Stock_Synthesis_files", "Model19.1 (22)", "MCMC", "ss.par"),
                overwrite = TRUE)

# Run MCMC test
starter <- r4ss::SS_readstarter(file = here::here("Stock_Synthesis_files", "Model19.1 (22)", "MCMC", "starter.ss"))
starter$MCMCburn <- 100
r4ss::SS_writestarter(starter, 
                      dir = here::here("Stock_Synthesis_files", "Model19.1 (22)", "MCMC"), 
                      file = "starter.ss",
                      overwrite = TRUE)

r4ss::run(dir = here::here("Stock_Synthesis_files", "Model19.1 (22)", "MCMC"), 
          extras = "-mcmc 1000 -mcsave 2",
          skipfinished = FALSE,
          show_in_console = TRUE)

r4ss::run(dir = here::here("Stock_Synthesis_files", "Model19.1 (22)", "MCMC"), 
          extras = "-mceval",
          skipfinished = FALSE,
          show_in_console = TRUE)

# Run full MCMC
# 
# starter <- r4ss::SS_readstarter(file = here::here("Stock_Synthesis_files", "Model19.1 (22)", "MCMC", "starter.ss"))
# starter$MCMCburn <- 10000
# r4ss::SS_writestarter(starter, 
#                       dir = here::here("Stock_Synthesis_files", "Model19.1 (22)", "MCMC"), 
#                       file = "starter.ss",
#                       overwrite = TRUE)
# 
# r4ss::run(dir = here::here("Stock_Synthesis_files", "Model19.1 (22)", "MCMC"), 
#           extras = "-mcmc 1000000 -mcsave 2000",
#           skipfinished = FALSE,
#           show_in_console = TRUE)
# 
# r4ss::run(dir = here::here("Stock_Synthesis_files", "Model19.1 (22)", "MCMC"), 
#           extras = "-mceval",
#           skipfinished = FALSE,
#           show_in_console = TRUE)

# Read output
mcmc <- r4ss::SSgetMCMC(here::here("Stock_Synthesis_files", "Model19.1 (22)", "MCMC"))

# Save output
save(mcmc, file = here::here("output", "mcmc.RData"))


