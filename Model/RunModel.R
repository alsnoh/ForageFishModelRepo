#######################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Forage Fish Ingestion Model  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#######################################################################################################################
# Predicts the length of fish (currently sandeel) across one growth season as a function of assimilated energy, which is modelled with a functional response for visual foraging
# It is based on Agnes Olin's model but with many simplifications, including using a structure based on the von bertalanffy equation

#~~~~~~~~~~~ INITIAL SETUP ~~~~~~~~~~#

# clear environment
rm(list = ls())


#Julian days at start and end of model
JD_ADDED <- 141 #141 
JD_FINISH <- 212 #212

# initial weight and length
#linear regression parameters for length-weight relationship
a1 <- 0.0028#exp(-6.8488)
a2 <- 3#3.4943
W0 <- 0.18 # 0.14 initial weight in g
L0 <- (W0/a1)^(1/a2) # initial length in cm 

# parameters
MaxWEIGHT <- 5 # master trait 
MaxLENGTH <- 20 # master trait
k <- 1#/MaxWEIGHT # growth rate  0.025
mu <- 0.1
lambda <- 0.5 


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

#source("Model/CalculateMaxWeight.R")



# #temperature-dependent assimilation
# assimilationV <- c()
# for(iday in 1:length(input_id)) {
#     assimilationV[iday] = (A1 + A2*temp[iday])-Ua   # tempConst for controlled experiments, temp for data
# }

# metabolismV <- c()
# for(iday in 1:length(input_id)) {
#     metabolismV[iday] =  M_FEED*pow(Q10_MF , temp[iday] / 10) 
# }

energy <- W0 * ED
weight <- W0
length <- L0
# Main model loop, calculating model results for each year
for (iyear in 1:1) {  #  1:length(ModelRunLengths)


    NoDays <- ModelRunLengths[iyear]
    current_year <- rep(prey_abundance[1 + NoDays * (iyear - 1),2], NoDays)

    # Calculate max weight
    #MaxWEIGHT <- CalculateMaxWeight(iyear, NoDays, assimilationV, WEIGHT)

    results_DF <- CalculateAssimilation(iyear, 
                                        NoDays, 
                                        MaxWEIGHT, 
                                        MaxLENGTH,
                                        tempConst, 
                                        #assimilationV,
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
                                        lambda,
                                        length,
                                        weight,
                                        energy)

    # Reset initial conditions every year or leave the same if you want to see the effect of growth over several years
    weight <- W0 #results_DF$weight[JD_FINISH] # W0
    length <- L0 #results_DF$length[JD_FINISH] # L0
    energy <- W0 * ED #weight * ED # W0 * ED
    results_daily_year <- data.frame(year = current_year, assimilated_weight = results_DF$assimilated_weight, ingested_weight = results_DF$ingested_weight, Weight = results_DF$weight, Length = results_DF$length, JulianDay = results_DF$jd, feeding_hours = results_DF$feeding_hours)
    DF <- rbind(DF,results_daily_year)
    

}


source("Model/saveResults.R")




  
