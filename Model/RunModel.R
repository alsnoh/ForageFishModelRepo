#######################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Forage Fish Ingestion Model  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#######################################################################################################################
# Predicts the length of fish (currently sandeel) across one growth season as a function of assimilated energy, which is modelled with a functional response for visual foraging
# It is based on Agnes Olin's model but with many simplifications, including using a structure based on the von bertalanffy equation

#~~~~~~~~~~~ INITIAL SETUP ~~~~~~~~~~#

# clear environment
rm(list = ls())
#plot.new()

# Initial Conditions
JD_ADDED <- 141 
W0 <- 0.14
#linear regression parameters for length-weight relationship
a1 <- exp(-6.8488)
a2 <- 3.4943
L0 <- (W0/a1)^(1/a2) # initial length in cm 

# parameters
#assimilation <- 0.4
k <- 0.1
#MaxLength <- 12

DF <- data.frame()

#~~~~~~~~~~~ REQUIRED PACKAGES ~~~~~~~~~~#

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

# load location data
locations <- read.delim("data/locations.csv")

# pick location "FoF", "DB", "Shetland", "ECG"
scenario <- "ECG"

# load constants
CONSTANTS <- read.csv("Model/CONSTANTS.csv")

# setting up header file with constants
source("Model/HeaderFile.R")
source("Model/CONSTANTS.R")

# setting up food and light data
source("Model/EnvironmentalConditions.R")

# loading function to calculate predicted length for one growth season, based on von bertalanffy
source("Model/CalculateAssimilation.R")

#source("Model/CalculateMaxWeight.R")
MaxWEIGHT <- 5



# function used in model loop

#temperature-dependent assimilation

assimilationV <- c()
for(iday in 1:length(input_id)) {
    assimilationV[iday] = (A1 + A2*tempConst[iday])-Ua
}

source("Model/getr.R")

WEIGHT <- W0
LENGTH <- L0
# Main model loop, calculating model results for each year
for (iyear in 1:length(ModelRunLengths)) {


    NoDays <- ModelRunLengths[iyear]
    current_year <- rep(prey_abundance[1 + NoDays * (iyear - 1),2], NoDays)

    # Calculate max weight
    #MaxWEIGHT <- CalculateMaxWeight(iyear, NoDays, assimilationV, WEIGHT)

    results_DF <- CalculateAssimilation(iyear, NoDays, MaxWEIGHT, assimilationV)

    # Reset initial conditions every year
    WEIGHT <- W0
    LENGTH <- L0
    results_daily_year <- data.frame(year = current_year, assimilated_energy = results_DF$assimilated_energy, ingested_energy = results_DF$ingested_energy, Weight = results_DF$weight, Length = results_DF$length, JulianDay = results_DF$jd)
    DF <- rbind(DF,results_daily_year)

}


source("Model/saveResults.R")




  
