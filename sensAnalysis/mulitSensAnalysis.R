rm(list = ls())
# Run Agnes' model and copy results into this directory

# Select which parameters to change (refer to rows in CONSTANTS.csv and then -1)
pars <- c(1,19)

# set multiples of 10% change (set n <- c(1,2) if testing min and max value)
n <- c(-5:5)

# Create data frames for metrics
NS_trend <- data.frame()
FoF_trend <- data.frame()
ECG_FoF_ratios <- data.frame()
Shetland_trend <- data.frame()

# Read in constants once
CONSTANTS_original <- read.csv("Model/CONSTANTS.csv")

for (jj in n) {
    CONSTANTS <- data.frame(CONSTANTS_original)
    CONSTANTS[pars[1], "value"] <- CONSTANTS[pars[1], "value"] * (1 + jj * 0.1)
    for (kk in n) {
        CONSTANTS[pars[2], "value"] <- CONSTANTS_original[pars[2], "value"]

        # Adjust parameter ±10% increments
        CONSTANTS[pars[2], "value"] <- CONSTANTS[pars[2], "value"] * (1 + kk * 0.1)
        # CONSTANTS[par, "lower"] <- CONSTANTS[par, "lower"] * (1 + j * 0.1)
        # CONSTANTS[par, "upper"] <- CONSTANTS[par, "upper"] * (1 + j * 0.1)

        # Adjust parameter to max and min value
        # range <- c(CONSTANTS[par, "lower"], CONSTANTS[par, "upper"])
        # CONSTANTS[par, "value"] <- range[j]
        

        # Write adjusted constants
        write.csv(CONSTANTS, "Model/CONSTANTS.csv", row.names = FALSE, quote = FALSE)

        # Run model
        source("Model/RunModel.R")
        
        # Restore constants
        write.csv(CONSTANTS_original, "Model/CONSTANTS.csv", row.names = FALSE, quote = FALSE)

        
        ECG <- read.csv("Results/ECG.csv")
        #FoF <- read.csv("Results/FoF.csv") 
        #Shetland <- read.csv("Results/Shetland.csv")
        #DB <- read.csv("Results/DB.csv")

        #1: Trend in North Sea Herring
        NS <- ECG[ECG$JulianDay == 300, ]
        NS_trend_val <- coef(lm(Length ~ year, data = NS))[2]
        NS_trend <- rbind(NS_trend, data.frame(A1 = CONSTANTS[pars[1], "value"], d1 = jj, k = CONSTANTS[pars[1], "value"], d2 = kk, trend = NS_trend_val))


        #1: Trend in Firth of Forth
        # FoF_182 <- FoF[FoF$JulianDay == 182, ]
        # FoF_trend_val <- coef(lm(Length ~ year, data = FoF_182))[2]
        # FoF_trend <- rbind(FoF_trend, data.frame(param = par, delta = j, trend = FoF_trend_val))

        # # 2: Ratio ECG/FoF
        # ECG_182 <- ECG[ECG$JulianDay == 182, ]
        # ECG_FoF_ratio <- mean(ECG_182$Length / FoF_182$Length)
        # # avgLength <- aggregate(predLength ~ loc, data = DF, FUN = mean, na.rm = TRUE)
        # # ECG_FoF_ratio <- avgLength[avgLength$loc == "ECG", "predLength"] /
        # #                  avgLength[avgLength$loc == "FoF", "predLength"]
        # ECG_FoF_ratios <- rbind(ECG_FoF_ratios, data.frame(param = par, delta = j, ratio = ECG_FoF_ratio))

        # # # 3: Trend in Shetland
        # Shetland_212 <- Shetland[Shetland$JulianDay == 212, ]
        # Shetland_trend_val <- coef(lm(Length ~ year, data = Shetland_212))[2]
        # Shetland_trend <- rbind(Shetland_trend, data.frame(param = par, delta = j, trend = Shetland_trend_val))


    }
}

write.csv(NS_trend, "Results/NS_trend_A1_k.csv", row.names = FALSE)

