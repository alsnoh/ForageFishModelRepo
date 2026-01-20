suppressMessages(library(ggplot2))
suppressMessages(library(ggpubr))
suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))
suppressMessages(library(readr))
suppressMessages(library(data.table))
suppressMessages(library(dplyr))

JD_ADDED = 141

# Different regions considered
locations <- c(scenario)#, "DB", "Shetland", "ECG")
colours <- c("goldenrod2", "#a04e2b", "#7388a6",  "grey")

DF1 <- data.frame()
DF2 <- data.frame()

for (location in locations){

    #################### obs length #############################################
    # Set up length data as data frame
    lengths <- fread(paste0("data/lengthData/length_", location, ".csv"))

    if (lengths$length[1] > 30)
        lengths$length <- lengths$length / 10

    # Consider just year and length data (cm)
    lengths <- data.frame(year = lengths$year, length = lengths$length)
    lengths$loc <- location

    realmaxLength <- max(lengths$length, na.rm = TRUE)

    print(realmaxLength)

    #################### Pred Length #############################################
    # Retrieve predicted lengths
    predLengths <- fread(paste0("AgnesModelResults/", location, ".csv"))

    # Consider just year and length data (cm)
    predLengths <- data.frame(year = predLengths$year, length = predLengths$Length)

    predLengths <- aggregate(predLengths$length, by = list(year = predLengths$year), FUN = mean, na.rm = TRUE)
    names(predLengths)[2] <- "predLength"

   
    #predLengths_length <- merge(predLengths, lengths)
    predLengths$loc <- location

    DF1 <- rbind(DF1, lengths)
    DF2 <- rbind(DF2, predLengths)

}

DF1$loc <- factor(DF1$loc, levels = locations)
DF2$loc <- factor(DF2$loc, levels = locations)

length_DF <- aggregate(length_DF$length, by = list(year = length_DF$year), FUN = mean, na.rm = TRUE)
names(length_DF)[2] <- "length"
#length_DF <- head(length_DF, -1)

#DF <- data.frame(year = DF$year, predLength = DF$predLength)#, length = length_DF$length)#, lengthDifference = DF$length-length_DF$length)


# plt <- ggplot() +
#   geom_point(data = DF, aes(x = year, y = length, colour = loc, shape = loc)) +
#   scale_colour_manual(values = c("goldenrod2", "#a04e2b", "#7388a6",  "grey"), name = "", guide = "none") +
#   scale_shape_manual(values = c(1, 2, 7, 5), name = "") +
#   labs(x = "year", y = "Observed length (cm)") +
#   theme_classic(base_size = 15) +
#   theme(legend.position = "bottom",
#         legend.margin = margin(c(1,1,1,1)))

        

plt <- ggplot() +
  geom_point(data = DF2, aes(x = year, y = predLength), colour = "goldenrod2") +
  geom_line(data = DF2, aes(x = year, y = predLength), colour = "goldenrod2") +
  geom_point(data = DF1, aes(x = year, y = length), colour = "#a04e2b") +
  geom_line(data = DF1, aes(x = year, y = length), colour = "#a04e2b") +
  geom_point(data = length_DF, aes(x = year, y = length)) +
  geom_line(data = length_DF, aes(x = year, y = length)) +
  labs(x = "year", y = "Predicted length (cm)") +
  theme_classic(base_size = 15) +
  theme(legend.position = "bottom",
        legend.margin = margin(c(1,1,1,1)),
        panel.grid = element_line(color = "#8ccde3",
                                  size = 0.75,
                                  linetype = 2)) +
  scale_x_continuous(breaks = seq(1973, 2016, by = 4))


# for (location in locations){

#   loc_index <- match(location, locations)

#   plt3 <- ggplot() +
#     geom_point(data = DF[DF$loc == location, ], colour = colours[loc_index], shape = 17, aes(x = year, y = predLength)) +
#     geom_point(data = DF[DF$loc == location, ], colour = colours[loc_index], shape = 2, aes(x = year, y = length), size = 2.5, alpha = 0.6) +
#     #scale_shape_manual(values = c(1,8), labels = c("Predicted", "Observed")) +
#     labs(x = "year", y = "Length (cm)", title = location) +
#     theme_classic(base_size = 20) +
#     theme(legend.position = "bottom",
#           legend.margin = margin(c(1,1,1,1)))
  
#   figure_name = paste0("figures/length_compare_", location, ".png")

#   ggsave(figure_name, plot = plt3, width = 25, height = 15, unit = "cm")

# }


 ggsave("figures/length_v_year_v1_eps1.png", plot = plt, width = 18, height = 20, unit = "cm")
# ggsave("figures/pred_length_v_year.png", plot = plt2, width = 18, height = 20, unit = "cm")