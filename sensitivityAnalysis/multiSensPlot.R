suppressMessages({
  library(ggplot2)
  library(ggpubr)
  library(lubridate)
  library(tidyverse)
  library(readr)
  library(data.table)
  library(dplyr)
  library(plotly)
})

ratio <- read.csv("Results/ECG_FoF_ratios_v&Dfrac.csv")
FoFtrends <- read.csv("Results/FoF_trend_v&Dfrac.csv")
ShetTrends <- read.csv("Results/Shetland_trend_v&Dfrac.csv")

DFs <- c(ratio, FoFtrends, ShetTrends)

nom_values <- c(1.329702731361, -0.0223533605494731, 0.0335363907825313)

names <- c("ratio", "FoF_trend", "Shetland_trend")

for (ii in seq.int(1, length(DFs) / 5)) {

  DF <- data.frame(delta1 = DFs[3 + 5 * (ii - 1)], delta2 = DFs[4 + 5 * (ii - 1)], metric = DFs[5 + 5 * (ii - 1)])
  # Get correct nominal value and rename last column to make life easier
  nom_value <- nom_values[ii]
  names(DF)[3] <- "metric"

#   DF$perDiff <- ((DF$metric - nom_value) * 100) / nom_value
#   DF[7, 4] <- NA  # correction
#   DF[7, 3] <- NA

  # Ensure `param` exists and is a factor
  # (If not, replace DF$param with the appropriate column name)
  #DF$param <- factor(DF$param, levels = unique(DF$param))


  p <- ggplot(DF, aes(x = delta1, y = delta2, z = metric)) +
    geom_contour_filled() 
    # geom_point(aes(colour = metric), position = position_dodge(width = 0.6)) +
    # theme_classic() +
    # theme(
    #   panel.grid.major.x = element_line(color = "#000000",
    #                             linewidth = 0.1,
    #                             linetype = 2),
    #   legend.position = "bottom",
    #   axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    # )

# p <- plot_ly(DF, x = ~delta1, y = ~delta2, z = ~metric, type = "scatter3d", mode = "markers",
#              marker = list(size = 5, color = ~metric, colorscale = "Viridis", symbol = "circle")) %>%
#   layout(title = "Customized 3D Scatter Plot",
#          scene = list(xaxis = list(title = "X Axis"),
#                       yaxis = list(title = "Y Axis"),
#                       zaxis = list(title = "Z Axis",
#                                    backgroundcolor = "rgb(230, 230,230)",
#                                    gridcolor = "rgb(255, 255, 255)",
#                                    showbackground = TRUE)))
  
  file_name <- paste0("figures/sensAnalysis/sens_analysisMulti_", names[ii], ".png")
  
  ggsave(file_name, plot = p, width = 18, height = 12, units = "cm")
}