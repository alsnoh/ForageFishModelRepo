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


# Read in constants once
CONSTANTS_original <- read.csv("Model/CONSTANTS.csv")

for (itemp in c(0)) {
    for (iprey_field_comp in c(0)) {
        for (iA1 in c(1,5)) {
            for (idecade in c(1)) {
                for (ifeeding_mode in c(1)) { # 1 is both, 2 is filter only, 3 is particulate only
                    
                    CONSTANTS <- data.frame(CONSTANTS_original)

                    CONSTANTS[28, "value"] <- itemp
                    CONSTANTS[29, "value"] <- iprey_field_comp
                    CONSTANTS[1, "value"] <- iA1
                    CONSTANTS[31, "value"] <- idecade
                    CONSTANTS[30, "value"] <- ifeeding_mode

                    # Write adjusted constants
                    write.csv(CONSTANTS, "Model/CONSTANTS.csv", row.names = FALSE, quote = FALSE)

                    # Run model
                    source("Model/RunModel.R")

                    # Restore constants
                    write.csv(CONSTANTS_original, "Model/CONSTANTS.csv", row.names = FALSE, quote = FALSE)
                }
            }
        }
    }
}