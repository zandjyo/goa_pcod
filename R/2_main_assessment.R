## Script to run 2022 GOA Pacific Cod Assessment (P. Hulson)

#######################################################################################
######## Load required packages & define parameters

libs <- c("data.table",
          "dplyr",
          "ggplot2",
          "magrittr", 
          "r4ss")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}

lapply(libs, library, character.only = TRUE)

# Current model name
Model_name <- "Model19.1 (22)"

# Current assessment year
new_SS_dat_year <- as.numeric(format(Sys.Date(), format = "%Y"))

#######################################################################################
######## Run base model

model_dir <- here::here("Stock_Synthesis_files", Model_name)

r4ss::run(dir = model_dir,
          skipfinished = FALSE,
          show_in_console = TRUE)

# read the model output and print diagnostic messages
model_run <- r4ss::SS_output(dir = model_dir,
                    verbose = TRUE,
                    printstats = TRUE)

# Get ssb and index fit for spreadsheets with figures
model_run$timeseries$Yr %>% 
  bind_cols(model_run$timeseries$SpawnBio / 2) %>% 
  rename(year = ...1, ssb = ...2) %>% 
  filter(year >= 1977,
         year <= new_SS_dat_year + 1) -> ssb_pred

model_run$timeseries$Yr %>% 
  bind_cols(model_run$timeseries$Bio_all) %>% 
  rename(year = ...1, biom = ...2) %>% 
  filter(year >= 1984,
         year <= new_SS_dat_year + 1) -> tot_trwl_pred

# Save output
save(model_run, file = here::here("output", "model_run.RData"))
write.csv(ssb_pred, here::here("output", "ssb_pred.csv"))
write.csv(tot_trwl_pred, here::here("output", "tot_trwl_pred.csv"))

#######################################################################################
######## Run retrospective analysis

# Define how many retro years you want to go back
ret_yr <- 1 # For testing
# ret_yr <- 10 # For full

# Run retrospective
r4ss::retro(dir = model_dir,
            oldsubdir = "",
            newsubdir = "retrospectives",
            years = 0:-ret_yr)

# load the retrospective models
retroModels <- r4ss::SSgetoutput(dirvec = file.path(
  model_dir, "retrospectives",
  paste("retro", 0:-ret_yr, sep = "")))

# summarize the model results
retroSummary <- r4ss::SSsummarize(retroModels)

# Run ss3diags on retro results
endyrvec <- retroSummary[["endyrs"]] + 0:-ret_yr
rho_output_ss3diags <- ss3diags::SSplotRetro(retroSummary,
                                             subplots = c("SSB"),
                                             endyrvec = endyrvec,
                                             legendlabels = paste("Data", 0:-ret_yr, "years"),
                                             print = TRUE,
                                             plotdir = here::here("plots", "nonSS"),
                                             pwidth = 8.5,
                                             pheight = 4.5)

# Save output
save(retroSummary, file = here::here("output", "retroSummary.RData"))
write.csv(retroSummary$SpawnBio, here::here("output", "retro_SSB.csv"))
write.csv(retroSummary$recruits, here::here("output", "retro_Rec.csv"))
write.csv(rho_output_ss3diags, here::here("output", "retro_Rho_ss3diags.csv"))

## All the r4ss retrospective stuff kinda sux but keeping it here to maybe use in future
# r4ss::SSplotComparisons(retroSummary,
#                         endyrvec = endyrvec,
#                         legendlabels = paste("Data", 0:-10, "years"),
#                         print = TRUE,
#                         plotdir = here::here("plots", "retro"))
# # calculate Mohn's rho
# rho_output_r4ss <- r4ss::SSmohnsrho(summaryoutput = retroSummary,
#                                endyrvec = endyrvec,
#                                startyr = retroSummary[["endyrs"]] - 10,
#                                verbose = FALSE)
#write.csv(rho_output_r4ss, here::here("output", "retro_Rho_r4ss.csv"))


#######################################################################################
######## Run management scenarios

source(here::here("R", "assessment", "run_mngmnt_scenarios.r"))

# Run management scenarios function
mscen <- Do_AK_Scenarios(Model_name = Model_name,
                         CYR = new_SS_dat_year,
                         SYR = 1977,
                         FCASTY = 15,
                         FLEETS = c(1:3),
                         do_fig = FALSE,
                         SEXES = 1)

# Save output
save(mscen, file = here::here("output", "mgmnt_scen.RData"))
write.csv(mscen$Tables, here::here("output", "mgmnt_scen_table.csv"))
write.csv(mscen$Two_year, here::here("output", "mgmnt_exec_summ.csv"))


#######################################################################################
######## Run Leave-One-Out Analysis

# Define how many LOO years you want to go back
loo_yr <- 1 # For testing
# loo_yr <- 10 # For full

ss_datname <- list.files(here::here("output"), pattern = "GOAPcod")

source(here::here("R", "assessment", "LeaveOneOut.r"))

LOO <- SS_doLOO(Model_name = Model_name,
                newsubdir = "LeaveOneOut", 
                years = 0:-loo_yr,
                datafilename = ss_datname,
                CYR = new_SS_dat_year)

# Save output
save(LOO, file = here::here("output", "LOO.RData"))
write.csv(LOO$Tables, here::here("output", "LOO_table.csv"))


#######################################################################################
######## Run Jitter Test

# Define how many jitters you want to do
Njitter <- 2 # For testing
# Njitter <- 50 # For full

# define a new directory
if (!file.exists(here::here("Stock_Synthesis_files", Model_name, "jitter"))) 
  dir.create(here::here("Stock_Synthesis_files", Model_name, "jitter"))

jitter_dir <- here::here("Stock_Synthesis_files", Model_name, "jitter")

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
jitter_loglike <- r4ss::jitter(dir = jitter_dir,
                               Njitter = Njitter,
                               jitter_fraction = 0.05)

# Save output
write.csv(jitter_loglike, here::here("output", "jitter_table.csv"))


#######################################################################################
######## Run MCMC

# Define burnin and length of chain
starter$MCMCburn <- 100 # For testing
chain <- 1000 # For testing
save <- 2 # For testing
# starter$MCMCburn <- 10000 # For full
# chain <- 1000000 # For full
# save <- 2000 # For testing

mcmc_dir <- here::here("Stock_Synthesis_files", Model_name, "MCMC")

# Write SS files in MCMC subfolder
r4ss::copy_SS_inputs(dir.old = model_dir, 
                     dir.new = mcmc_dir,
                     overwrite = TRUE)
base::file.copy(from = here::here("Stock_Synthesis_files", Model_name, "ss.exe"),
                to = here::here("Stock_Synthesis_files", Model_name, "MCMC", "ss.exe"),
                overwrite = TRUE)
base::file.copy(from = here::here("Stock_Synthesis_files", Model_name, "ss.par"),
                to = here::here("Stock_Synthesis_files", Model_name, "MCMC", "ss.par"),
                overwrite = TRUE)

# Run MCMC
starter <- r4ss::SS_readstarter(file = here::here("Stock_Synthesis_files", Model_name, "MCMC", "starter.ss"))

r4ss::SS_writestarter(starter, 
                      dir = here::here("Stock_Synthesis_files", Model_name, "MCMC"), 
                      file = "starter.ss",
                      overwrite = TRUE)

r4ss::run(dir = here::here("Stock_Synthesis_files", Model_name, "MCMC"), 
          extras = paste0("-mcmc ", chain," -mcsave ", save),
          skipfinished = FALSE,
          show_in_console = TRUE)

r4ss::run(dir = here::here("Stock_Synthesis_files", Model_name, "MCMC"), 
          extras = "-mceval",
          skipfinished = FALSE,
          show_in_console = TRUE)

# Read output
mcmc <- r4ss::SSgetMCMC(here::here("Stock_Synthesis_files", Model_name, "MCMC"))

# Save output
save(mcmc, file = here::here("output", "mcmc.RData"))


