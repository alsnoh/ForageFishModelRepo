# Run Agnes' model and copy results into this directory

# Select which parameters to change (refer to rows in CONSTANTS_copy.csv and then -1)
pars <- c(5,7)

# set multiples of 10% change (set n <- c(1,2) if testing min and max value)
n <- c(-5:5)

# Create data frames for metrics
FoF_trend <- data.frame()
ECG_FoF_ratios <- data.frame()
Shetland_trend <- data.frame()

# Define directories laptop
model_dir <- "C:/Users/Alex/OneDrive - University of Strathclyde/Code/sandeel_model/sandeel_model/"
results_copy_dir <- "C:/Users/Alex/OneDrive - University of Strathclyde/Code/PhD/Learning stats/AgnesModelResults/"
analysis_dir <- "C:/Users/Alex/OneDrive - University of Strathclyde/Code/PhD/Learning stats/"

# Define relative directories PC
# model_dir <- "C:/Users/cgb25176/OneDrive - University of Strathclyde/Code/sandeel_model/sandeel_model"
# results_copy_dir <- "C:/Users/cgb25176/OneDrive - University of Strathclyde/Code/PhD/Learning stats/AgnesModelResults/"
# analysis_dir <- "C:/Users/cgb25176/OneDrive - University of Strathclyde/Code/PhD/Learning stats/"


# Read in constants_copy once
CONSTANTS_original <- read.csv(file.path(model_dir, "CONSTANTS.csv"))

for (jj in n) {
    CONSTANTS_copy <- data.frame(CONSTANTS_original)
    CONSTANTS_copy[pars[1], "value"] <- CONSTANTS_copy[pars[1], "value"] * (1 + jj * 0.1)
    for (kk in n) {

        CONSTANTS_copy[pars[2], "value"] <- CONSTANTS_original[pars[2], "value"]

        # Adjust parameter ±10% increments
        CONSTANTS_copy[pars[2], "value"] <- CONSTANTS_copy[pars[2], "value"] * (1 + kk * 0.1)
        # CONSTANTS_copy[par, "lower"] <- CONSTANTS_copy[par, "lower"] * (1 + j * 0.1)
        # CONSTANTS_copy[par, "upper"] <- CONSTANTS_copy[par, "upper"] * (1 + j * 0.1)

        # Adjust parameter to max and min value
        # range <- c(CONSTANTS_copy[par, "lower"], CONSTANTS_copy[par, "upper"])
        # CONSTANTS_copy[par, "value"] <- range[j]
        

        # Write adjusted constants_copy
        write.csv(CONSTANTS_copy, file.path(model_dir, "CONSTANTS.csv"), row.names = FALSE, quote = FALSE)

        # Run model
        setwd(model_dir)
        source("ModelRun.R")
        

        # Copy results
        file.copy(
        from = file.path(model_dir, "Results_alex", c("DB.csv", "ECG.csv", "FoF.csv", "Shetland.csv")),
        to = results_copy_dir,
        overwrite = TRUE
        )

        setwd(analysis_dir)

        # Restore constants_copy
        #write.csv(CONSTANTS_original, file.path(model_dir, "CONSTANTS.csv"), row.names = FALSE, quote = FALSE)

        # Analyse results
        source("obs_length_v_pred_length.R")

        ECG <- subset(DF, loc == "ECG")
        FoF <- subset(DF, loc == "FoF")
        Shetland <- subset(DF, loc == "Shetland")
        DB <- subset(DF, loc == "DB")

        # 1: Trend in Firth of Forth
        FoF_trend_val <- coef(lm(predLength ~ year, data = FoF))[2]
        FoF_trend <- rbind(FoF_trend, data.frame(param1 = pars[1], param2 = pars[2], delta1 = jj, delta2 = kk, trend = FoF_trend_val))

        # 2: Ratio ECG/FoF
        avgLength <- aggregate(predLength ~ loc, data = DF, FUN = mean, na.rm = TRUE)
        ECG_FoF_ratio <- avgLength[avgLength$loc == "ECG", "predLength"] /
                        avgLength[avgLength$loc == "FoF", "predLength"]
        ECG_FoF_ratios <- rbind(ECG_FoF_ratios, data.frame(param1 = pars[1], param2 = pars[2], delta1 = jj, delta2 = kk, ratio = ECG_FoF_ratio))

        # 3: Trend in Shetland
        Shetland_trend_val <- coef(lm(predLength ~ year, data = Shetland))[2]
        Shetland_trend <- rbind(Shetland_trend, data.frame(param1 = pars[1], param2 = pars[2], delta1 = jj, delta2 = kk, trend = Shetland_trend_val))

        setwd(model_dir)
    }
}

# Restore constants_copy
write.csv(CONSTANTS_original, file.path(model_dir, "CONSTANTS.csv"), row.names = FALSE, quote = FALSE)

# Return to analysis dir at the end
setwd(analysis_dir)

write.csv(FoF_trend, "Results/FoF_trend_v&Dfrac.csv", row.names = FALSE)
write.csv(ECG_FoF_ratios, "Results/ECG_FoF_ratios_v&Dfrac.csv", row.names = FALSE)
write.csv(Shetland_trend, "Results/Shetland_trend_v&Dfrac.csv", row.names = FALSE)