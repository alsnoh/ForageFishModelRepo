suppressMessages(library(ggplot2))
suppressMessages(library(ggpubr))
suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))
suppressMessages(library(readr))
suppressMessages(library(data.table))
suppressMessages(library(dplyr))

JD_ADDED = 141

# Different regions considered
locations <- c("FoF", "DB", "Shetland", "ECG")
colours <- c("goldenrod2", "#a04e2b", "#7388a6",  "grey")

lines <- list()
reg_summary <- data.frame()
DF <- data.frame()

for (location in locations){

    #################### Pred Length #############################################
    # Retrieve predicted lengths
    lengths <- fread(paste0("AgnesModelResults/", location, ".csv"))

    # Consider just year and length data (cm)
    lengths <- data.frame(year = lengths$year, length = lengths$Length)

    lengths <- aggregate(lengths$length, by = list(year = lengths$year), FUN = mean, na.rm = TRUE)
    names(lengths)[2] <- "length"

    #################### Prey Abundence #####################################
    prey <- fread(paste0("data/abundanceData/abundance_", location, ".csv"))

    # Take only dates where day is between 141 and 212
    prey <- prey %>% filter(prey$jd >= JD_ADDED & prey$jd <= 212)

    prey$totalEnergy <- rowSums(prey[, c(4:21)])

    prey <- aggregate(prey$totalEnergy, by = list(year = prey$year), FUN = mean, na.rm = TRUE)
    names(prey)[2] <- "totalEnergy"



    prey_length <- merge(prey, lengths)
    prey_length$loc <- location

    prey_length$totalEnergy <- prey_length$totalEnergy / 1000

    # Calculate regressions and plot
    reg <- lm(length ~ totalEnergy, data = prey_length)

    #get intercept and slope value
    coeff <- coefficients(reg)
    intercept <- coeff[1]
    slope <- coeff[2]
    r2 <- summary(reg)$r.squared

    loc_index <- match(location, locations)

    lines[[location]] <- geom_abline(intercept = intercept, slope = slope,
                                   color = colours[loc_index],
                                   linetype = "dashed", linewidth = 1.3)
    
    reg_summary <- rbind(reg_summary, data.frame(
    Location = location,
    R2 = sprintf("%.2f", r2),
    Slope = sprintf("%.2f", slope)
    ))

    DF <- rbind(DF, prey_length)
}

DF$loc <- factor(DF$loc, levels = locations)

summary_text <- paste0(
  paste0(
    sprintf("%s:  R² = %s,  slope = %s", 
            reg_summary$Location, reg_summary$R2, reg_summary$Slope)
  , collapse = "\n")
)

plt <- ggplot() + 
  geom_point(data = DF, aes(x = totalEnergy, y = length, colour = loc, shape = loc)) +
  lines +
  scale_colour_manual(values = colours, name = "") +
  scale_shape_manual(values = c(1, 2, 7, 5), name = "") +
  labs(x = "Mean prey energy", y = "Predicted length (cm)") +
  theme_classic(base_size = 15) +
  theme(legend.position = "bottom",
        legend.margin = margin(c(1,1,1,1))) +
  annotate("text", 
           x = Inf, y = -Inf, 
           label = summary_text, 
           hjust = 1.1, vjust = -0.5, 
           size = 5, fontface = "bold", family = "mono",
           color = "black") +
  annotate("rect", xmin = Inf, xmax = Inf, ymin = -Inf, ymax = -Inf, alpha = 0)



ggsave("figures/pred_length_v_energy.png", plot = plt, width = 22, height = 20, unit = "cm")
