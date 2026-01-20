#############################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  SANDEEL DEB MODEL  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#############################################################################################################


#~~~~~~~~~~~ INITIAL SETUP ~~~~~~~~~~#

# clear environment
#rm(list = ls())
plot.new()

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

#~~~~~~~~~~~ LOAD PARAMETER VALUES ~~~~~~~~~~#
  
CONSTANTS = read.csv(file.path(model_dir, "CONSTANTS.csv"))

source("initialConditions.R")



setwd(model_dir)

#~~~~~~~~~~~ MAKE MAP ~~~~~~~~~~#

locations = read.delim("EnvironmentalDrivers/locations.csv")

source("SupportingRScripts/map.R")

#if (file.exists("ModelRun.dll")) {
#  file.remove("ModelRun.dll")
#} 

total_energy_df = data.frame(loc = character(), year = numeric(), energy = numeric())





#~~~~~~~~~~~ Run model function ~~~~~~~~~~#
RunModel <- function(x, y) {

  scenario <- x

  runYear <- y

  
  #~~~~~~~~~~~ RUN SUPPORTING SCRIPTS ~~~~~~~~~~#
  
  # julian day of end of model run
  JD_FINISH = 212
  
  
  # setting up food and temperature data
  source("SupportingRScripts/EnvironmentalConditions.R")
  
  # setting up header file with constants
  source("SupportingRScripts/HeaderFile.R")
  
  if(is.numeric(scenario)) print(c(scenario/max(scenarios)))
  
  
  DEBrun = function()
  {
    {
      DEBmodel =  .C("ModelRun",
                     as.double(t(prey_abundance)),  # this is now a matrix that contains all abundance data
                     as.double(temp),               # temperature data
                     as.double(light),              # light data
                     as.integer(prey_mode),         # binary feeding mode
                     as.double(prey_energy),        # prey-specific energy content
                     as.double(prey_ed),            # prey-specific energy density
                     as.double(prey_size),          # pre-specific length
                     as.double(prey_image_area),    # pre-specific image area
                     as.double(a_c),                # diffuse attenuation coefficient
                     as.integer(JulianDayV),        # julian days
                     as.integer(DayLengths),        # length of days
                     as.double(R),                  # reserve energy
                     as.double(S),                  # structural energy 
                     as.integer(JD_ADDED),          # starting julian day of run
                     as.integer(JD_FINISH),         # finishing julian day of run
                     as.double(LENGTH),             # length
                     as.double(WEIGHT),             # weight
                     as.integer(ModelRunLengths),   # number of days in each run
                     result = double(length(1)))
    }
    DEBmodel[["result"]]
  }

  if (file.exists("ModelRun.dll")) {
    dyn.unload("ModelRun.dll")
  } 

  system("touch ModelRun.c", wait=T)
  
  # compile c code
  system('R CMD SHLIB ModelRun.c')
  
  # load .so file (.dll if windows) 
  dyn.load("ModelRun.dll")
  
  # running model
  DEBrun()
  
  source("SupportingRScripts/SaveResults.R")

 
  if (file.exists("ModelRun.dll")) {
    dyn.unload("ModelRun.dll")
  } 

  file.remove("ModelRun.dll")
  file.remove("ModelRun.o")
   
}