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
L0 <- 4 

# parameters
#assimilation <- 0.4
k <- 0.1
#MaxLength <- 12

length_DF <- data.frame()

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
scenario <- "FoF"

# load constants
CONSTANTS <- read.csv("Model/CONSTANTS.csv")

# setting up header file with constants
source("Model/HeaderFile.R")
source("Model/CONSTANTS.R")

# setting up food and light data
source("Model/EnvironmentalConditions.R")

# loading function to calculate predicted length for one growth season, based on von bertalanffy
source("Model/CalculateAssimilation.R")

source("Model/CalculateMaxLength.R")



# function used in model loop

#temperature-dependent assimilation

assimilationV <- c()
for(iday in 1:length(input_id)) {
    assimilationV[iday] = (A1 + A2*temp[iday])-Ua
}

source("Model/getr.R")

LENGTH <- L0
# Main model loop, calculating model results for each year
for (iyear in 1:length(ModelRunLengths)) {


    NoDays <- ModelRunLengths[iyear]
    current_year <- rep(prey_abundance[1 + NoDays * (iyear - 1),2], NoDays)
    i_dailys <- numeric(NoDays)
    LENGTH_daily <- numeric(NoDays)

    # Calculate max length
    MaxLength <- CalculateMaxLengh(iyear, NoDays, assimilationV, LENGTH)

    results_DF <- CalculateAssimilation(iyear, NoDays, i_dailys, LENGTH_daily, MaxLength, assimilationV)

    # Reset initial length every year
    LENGTH <- L0
    LENGTH_daily_year <- data.frame(year = current_year, assimilated_energy = results_DF$assimilated_energy, length = results_DF$length, jd = results_DF$jd)
    length_DF <- rbind(length_DF,LENGTH_daily_year)

}


# Plotting results
#source("generateFigures/PredictedLength.R")
source("messing about/obs_length_v_pred_length.R")


  
