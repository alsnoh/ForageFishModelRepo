suppressMessages({
  library(ggplot2)
  library(ggpubr)
  library(lubridate)
  library(tidyverse)
  library(readr)
  library(data.table)
  library(dplyr)
})

ratio <- read.csv("Results/ECG_FoF_ratios_MinMax.csv")
FoFtrends <- read.csv("Results/FoF_trend_MinMax.csv")
ShetTrends <- read.csv("Results/Shetland_trend_MinMax.csv")

DFs <- c(ratio, FoFtrends, ShetTrends)

# parameter names
labels <- c(
  expression(italic(alpha["ϵ"])), expression(italic(beta["ϵ"])), expression(italic("U"["ϵ"])),
  expression(italic("h")), expression(italic("v")), expression(italic("C")),
  expression(italic("D"["frac"])), expression(italic("K"["D"])), expression(italic("z")),
  expression(italic("b")), expression(italic("m")), expression(italic(alpha["dig"])),
  expression(italic(beta["dig"])), expression(italic(delta^"*")),
  expression(italic(alpha["gut"])), expression(italic(beta["gut"])),
  expression(italic(alpha["met"])), expression(italic(beta["met"])),
  expression(italic("Q"[10])), expression(italic("F")),
  expression(italic(zeta["SDA"])), expression(italic(alpha["alloc"])),
  expression(italic(beta["alloc"])), expression(italic(delta["S"])),
  expression(italic(delta["R"])), expression(italic(alpha["dry"])),
  expression(italic(beta["dry"])), expression(italic(omega["SDW"])),
  expression(italic(omega["RDW"]))
)

nom_values <- c(1.329702731361, -0.0223533605494731, 0.0335363907825313)

names <- c("ratio", "FoF_trend", "Shetland_trend")

for (ii in seq.int(1, length(DFs) / 3)) {

  DF <- data.frame(param = DFs[1 + 3 * (ii - 1)], delta = DFs[2 + 3 * (ii - 1)], metric = DFs[3 + 3 * (ii - 1)])
  # Get correct nominal value and rename last column to make life easier
  nom_value <- nom_values[ii]
  names(DF)[3] <- "metric"

  DF$perDiff <- ((DF$metric - nom_value) * 100) / nom_value
  DF[7, 4] <- NA  # correction
  DF[7, 3] <- NA

  # Ensure `param` exists and is a factor
  # (If not, replace DF$param with the appropriate column name)
  DF$param <- factor(DF$param, levels = unique(DF$param))


  # Plot
  p <- ggplot(DF, aes(x = param, y = metric)) +
    geom_point(aes(colour = factor(delta)), position = position_dodge(width = 0.6)) +
    scale_colour_manual(values = c("#d61717", "#30ce21"), name = "") +
    geom_hline(yintercept = nom_value, linetype = 3) +
    scale_x_discrete(labels = labels) +
    labs(x = "Parameter", y = names[ii]) +
    theme_classic() +
    theme(
      panel.grid.major.x = element_line(color = "#000000",
                                linewidth = 0.1,
                                linetype = 2),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
  
  file_name <- paste0("figures/sensAnalysis/sens_analysis2_", names[ii], ".png")

  ggsave(file_name, width = 18, height = 12, units = "cm")
}