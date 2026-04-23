rm(list = ls())
# set multiples of 10% change (set n <- c(1,2) if testing min and max value)
n <- c(1,2)

# Create data frames for metrics
FoF_trend <- data.frame()
ECG_FoF_ratios <- data.frame()
Shetland_trend <- data.frame()


# Read in constants once
CONSTANTS_original <- read.csv("Model/CONSTANTS.csv")

# Select which parameters to change (refer to rows in CONSTANTS.csv and then -1)
pars <- c(1:(nrow(CONSTANTS_original)-1)) 
#pars <- (1:1)

for (par in pars) {
  for (j in n) {

    CONSTANTS <- data.frame(CONSTANTS_original)

    # Adjust parameter ±10% increments
    # CONSTANTS[par, "value"] <- CONSTANTS[par, "value"] * (1 + j * 0.1)
    # CONSTANTS[par, "lower"] <- CONSTANTS[par, "lower"] * (1 + j * 0.1)
    # CONSTANTS[par, "upper"] <- CONSTANTS[par, "upper"] * (1 + j * 0.1)

    # Adjust parameter to max and min value
    range <- c(CONSTANTS[par, "lower"], CONSTANTS[par, "upper"])
    CONSTANTS[par, "value"] <- range[j]
    

    # Write adjusted constants
    write.csv(CONSTANTS, "Model/CONSTANTS.csv", row.names = FALSE, quote = FALSE)

    # Run model
    source("Model/RunModel.R")


    # Restore constants
    write.csv(CONSTANTS_original, "Model/CONSTANTS.csv", row.names = FALSE, quote = FALSE)


    ECG <- read.csv("Results/ECG.csv")
    FoF <- read.csv("Results/FoF.csv") 
    Shetland <- read.csv("Results/Shetland.csv")
    DB <- read.csv("Results/DB.csv")

    # 1: Trend in Firth of Forth
    FoF_182 <- FoF[FoF$JulianDay == 182, ]
    FoF_trend_val <- coef(lm(Length ~ year, data = FoF_182))[2]
    FoF_trend <- rbind(FoF_trend, data.frame(param = par, delta = j, trend = FoF_trend_val))

    # 2: Ratio ECG/FoF
    ECG_182 <- ECG[ECG$JulianDay == 182, ]
    ECG_FoF_ratio <- mean(ECG_182$Length / FoF_182$Length)
    # avgLength <- aggregate(predLength ~ loc, data = DF, FUN = mean, na.rm = TRUE)
    # ECG_FoF_ratio <- avgLength[avgLength$loc == "ECG", "predLength"] /
    #                  avgLength[avgLength$loc == "FoF", "predLength"]
    ECG_FoF_ratios <- rbind(ECG_FoF_ratios, data.frame(param = par, delta = j, ratio = ECG_FoF_ratio))

    # # 3: Trend in Shetland
    Shetland_212 <- Shetland[Shetland$JulianDay == 212, ]
    Shetland_trend_val <- coef(lm(Length ~ year, data = Shetland_212))[2]
    Shetland_trend <- rbind(Shetland_trend, data.frame(param = par, delta = j, trend = Shetland_trend_val))

  }
}


write.csv(FoF_trend, "Results/FoF_trend_MinMax.csv", row.names = FALSE)
#write.csv(FoF_trend, "Results/FoF_trend_fraction_of_dl.csv", row.names = FALSE)

write.csv(ECG_FoF_ratios, "Results/ECG_FoF_ratios_MinMax.csv", row.names = FALSE)
#write.csv(ECG_FoF_ratios, "Results/ECG_FoF_ratios_241.csv", row.names = FALSE)
write.csv(Shetland_trend, "Results/Shetland_trend_MinMax.csv", row.names = FALSE)
#write.csv(Shetland_trend, "Results/Shetland_trend_241.csv", row.names = FALSE)