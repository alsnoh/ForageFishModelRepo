suppressMessages(library(ggplot2))
suppressMessages(library(ggpubr))
suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))
suppressMessages(library(readr))
suppressMessages(library(data.table))

# Start day of model
JD_ADDED <- 141

# Different regions considered
locations <- c("FoF", "DB", "Shetland", "ECG")

DF <- data.frame()

for(location in locations){
    ################### temp ############################################
    # Set up temperature data as data frame
    temp <- fread(paste0("data/tempData/temp_", location, ".csv"))

    # Add new column with just year of each sample
    temp$year <- year(temp$date)

    day <- yday(temp$date)

    # Take only dates where day is between 141 and 212
    temp <- temp %>% filter(day >= JD_ADDED & day <= 212)

    # Aggregate data by taking average for each year and rename column
    temp <- aggregate(temp$temp, by = list(year = temp$year), FUN = mean, na.rm = TRUE)
    names(temp)[2] <- "temp"


    #################### length #############################################
    # Set up length data as data frame
    lengths <- fread(paste0("data/lengthData/length_", location, ".csv"))

    if(lengths$length[1] > 30)
    lengths$length <- lengths$length / 10

    # Consider just year and length data (cm)
    lengths <- data.frame(year = lengths$year, length = lengths$length)


  
    # Merge temp and length data frames by year and add new column for location
    temp_length <- merge(temp, lengths)
    temp_length$loc <- location

    DF <- rbind(DF, temp_length)


}

DF$loc <- factor(DF$loc, levels = locations)
################### Plotting #############################################
plt = ggplot() +
  geom_point(data = DF, aes(x = temp, y = length, colour = loc, shape = loc)) +
  scale_colour_manual(values = c("goldenrod2", "#a04e2b", "#7388a6",  "grey"), name = "", guide = "none") +
  scale_shape_manual(values = c(1, 2, 7, 5), name = "") +
  annotate("text", x = 10, y = 8.7, label = "Length\n1 July", colour = "grey35", size = 2.5) +
  annotate("text", x = 13.5, y = 11.7, label = "Length\noverwintering", colour = "grey35", size = 2.5) +
  lims(y = c(3, 12.5)) +
  labs(x = "Mean growth temperature (°C)", y = "Observed length (cm)") +
  theme_classic(base_size = 8) +
  theme(legend.position = "bottom",
        legend.margin=margin(c(1,1,1,1)))

ggsave("figures/length_v_temp.png", width = 18, height = 20, unit = "cm")