##########################################################################################
# Forage Fish Model - Assimilation Simulation
# This script runs the assimilation model for a range of prey abundances, to explore how 
# assimilation and feeding strategy changes with abundance of prey and light conditions.
# all conditions are simulated and light is assumed constant throughout day to mimic lab
###########################################################################################

suppressMessages(library(geosphere))
suppressMessages(library(lubridate))
suppressMessages(library(sp))
suppressMessages(library(sf))
suppressMessages(library(ggplot2))
suppressMessages(library(ggpubr))
suppressMessages(library(dplyr))
suppressMessages(library(colorspace))
suppressMessages(library(scales))
suppressMessages(library(nlme))
suppressMessages(library(MuMIn))
suppressMessages(library(jsonlite))

# clear environment
rm(list = ls())

# initial weight and length
#linear regression parameters for length-weight relationship
a1 <- 0.0028#exp(-6.8488)
a2 <- 3.3#3.4943
W0 <- 6 # 0.18 initial weight in g
L0 <- (W0/a1)^(1/a2) # initial length in cm 

# parameters
k <- 1.7#/MaxWEIGHT # g
mu <- 0
lambda <- 0.5 

# load constants
CONSTANTS <- read.csv("Model/CONSTANTS.csv")

# setting up header file with constants
source("Model/HeaderFile.R")
source("Model/CONSTANTS.R")

source("filter_partic/simulate_environment.R")

source("Model/CalculateAssimilation.R")

iyear <- 1
NoDays <- 1

DF <- data.frame()

for (abund in c(1,5,10,50,100,500,1000,5000,10000,50000,100000)) { #seq(1000,100000, by = 1000)

    energy <- W0 * ED
    weight <- W0
    length <- L0

    # simulate prey abundance for one prey type,
    # structure of prey matrix is so that is matches data structure which
    # calculateAssimilation expects
    prey_abundance = matrix(nrow = 1, ncol = NoTaxa+3)
    for (i in 1:(NoTaxa)) {
    prey_abundance[1,i+3] = abund
    }

    for (light in c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1, 10, 100)) { #c(0.01, 0.1, 1, 10, 100) #c(1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1, 10, 100, 1000)

        results_DF <- CalculateAssimilation(iyear, 
                                            NoDays, 
                                            temp, #tempConst for controlled experiments, temp for actual data
                                            prey_abundance, #prey_abundanceConst for controlled experiments, prey_abundance for actual data
                                            prey_size, 
                                            prey_energy, 
                                            prey_ed, 
                                            prey_mode, 
                                            prey_image_area,
                                            prey_name, 
                                            JulianDayV, 
                                            DayLengths, #DayLengthsConst for controlled experiments, DayLengths for actual data
                                            light, #lightConst
                                            a_c,
                                            mu,
                                            length,
                                            weight,
                                            energy)

        results <- data.frame(assimilated_energy = results_DF$assimilated_weight, particulates = results_DF$particulates, filters = results_DF$filters, percentage = results_DF$percentage_particulates, light = results_DF$light, abundance = results_DF$preyAbundance)
        DF <- rbind(DF,results)
    }
}
write.csv(DF, paste0("Results/light_", light[1], "_weight_", W0, " partic.csv"), row.names = F)