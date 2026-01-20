rm(list = ls())

analysis_dir <- "C:/Users/cgb25176/OneDrive - University of Strathclyde/Code/PhD/Learning stats/"
model_dir <- "C:/Users/cgb25176/OneDrive - University of Strathclyde/Code/sandeel_model/sandeel_model/"

calibration = F
trends = T
sensitivity = F
initial =  F
ENVsensitivity = F

setwd(analysis_dir)

scenarios <- c("DB")

runYears <- c(1982, 1985)

source("ModelRun.R")

for (scenario in scenarios) {

    # Remove years that aren't in data set
    prey_abundance = read.csv(paste0("EnvironmentalDrivers/Food/abundance_", scenario, ".csv"))
    runYears <- runYears[runYears %in% prey_abundance$year]

    if (length(runYears) == 0) {
        next
    }

    for (runYear in runYears) {
        RunModel(scenario, runYear)
    }
}

# source("SupportingRScripts/TrendPlots/TrendPlots.R")

print("Test")