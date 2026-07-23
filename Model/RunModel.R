#######################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Forage Fish Ingestion Model  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#######################################################################################################################
# Predicts the length of fish (currently sandeel) across one growth season as a function of assimilated energy, which is modelled with a functional response for visual foraging
# It is based on Agnes Olin's model but with many simplifications, including using a structure based on the von bertalanffy equation

#~~~~~~~~~~~ INITIAL SETUP ~~~~~~~~~~#

# clear environment
rm(list = ls())

#run mode (continuous 1 or single year 2)
mode <- 1

# const environmental conditions (1) or variable environmental conditions (2)
env_mode <- 2

temp_vec <- list()
prey_abundance_vec <- list()
DayLengths_vec <- list()
light_vec <- list()

weight_results <- list()
length_results <- list()
energy_results <- list()


#Julian days at start and end of model
jd_added <- c(1,141)
jd_finish <- c(365,212)
JD_ADDED <- jd_added[mode] 
JD_FINISH <- jd_finish[mode]

# initial weight and length
#linear regression parameters for length-weight relationship
a1 <- 0.0028#exp(-6.8488)
a2 <- 3.3#3.4943
W0 <- 6 # 0.18 initial weight in g
L0 <- (W0/a1)^(1/a2) # initial length in cm 
WAM <- 300 # weight at maturation

# parameters
#/MaxWEIGHT # g
mu <- 1
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
scenarios <- c("FoF")#,"DB", "Shetland", "ECG")#, "Shetland", "ECG")

# load constants
CONSTANTS <- read.csv("Model/CONSTANTS.csv")

# setting up header file with constants
source("Model/HeaderFile.R")
source("Model/CONSTANTS.R")


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
for (scenario in scenarios) {

    # setting up food and light data
    source("Model/EnvironmentalConditions.R")

    energy <- W0 * ED
    weight <- W0
    length <- L0
    # Main model loop, calculating model results for each year

    temp_vec[[1]] <- tempConst
    temp_vec[[2]] <- temp
    prey_abundance_vec[[1]] <- prey_abundanceConst
    prey_abundance_vec[[2]] <- prey_abundance
    DayLengths_vec[[1]] <- DayLengthsConst
    DayLengths_vec[[2]] <- DayLengths
    light_vec[[1]] <- lightConst
    light_vec[[2]] <- light

    for (iyear in 1:1) {  #  1:length(ModelRunLengths)


        NoDays <- ModelRunLengths[iyear]
        current_year <- rep(prey_abundance[1 + NoDays * (iyear - 1),2], NoDays)

        # Calculate max weight
        #MaxWEIGHT <- CalculateMaxWeight(iyear, NoDays, assimilationV, WEIGHT)

        results_DF <- CalculateAssimilation(iyear, 
                                            NoDays, 
                                            temp_vec[[env_mode]], #tempConst for controlled experiments, temp for actual data
                                            prey_abundance_vec[[env_mode]], #prey_abundanceConst for controlled experiments, prey_abundance for actual data
                                            prey_size, 
                                            prey_energy, 
                                            prey_ed, 
                                            prey_mode, 
                                            prey_image_area,
                                            prey_name, 
                                            JulianDayV, 
                                            DayLengths_vec[[env_mode]], #DayLengthsConst for controlled experiments, DayLengths for actual data
                                            light_vec[[env_mode]], #lightConst
                                            a_c,
                                            mu,
                                            length,
                                            weight,
                                            energy,
                                            WAM)

        # Reset initial conditions every year or leave the same if you want to see the effect of growth over several years
        weight_results[[1]] <- results_DF$weight[JD_FINISH]
        weight_results[[2]] <- W0
        length_results[[1]] <- results_DF$length[JD_FINISH]
        length_results[[2]] <- L0
        energy_results[[1]] <- results_DF$weight[JD_FINISH] * ED
        energy_results[[2]] <- W0 * ED

        weight <- weight_results[[mode]]  #results_DF$weight[JD_FINISH] # W0
        length <- length_results[[mode]]  #results_DF$length[JD_FINISH] # L0
        energy <- energy_results[[mode]]  #weight * ED # W0 * ED
        results_daily_year <- data.frame(year = current_year, assimilated_weight = results_DF$assimilated_weight, ingested_weight = results_DF$ingested_weight, Weight = results_DF$weight, Length = results_DF$length, JulianDay = results_DF$jd, feeding_hours = results_DF$feeding_hours, Metabolism = results_DF$metabolism, percentage_partic = results_DF$percentage_particulates, percentage_filters = results_DF$percentage_filters, percentage_hiding = results_DF$percentage_hiding, metaConst = results_DF$metaConst, assimilation = results_DF$assimilation, gape_size = results_DF$gape_size)
        DF <- rbind(DF,results_daily_year)
        if (weight == 0)
        {
            break
        }
        

    }


    source("Model/saveResults.R")
    DF <- data.frame()
}




  
