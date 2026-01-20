suppressMessages(library(readr))
suppressMessages(library(data.table))

# Combine FairIsle and Hermaness to make Shetland data
FairIsle <- fread("data/FairIsle.csv")

Hermaness <- fread("data/Hermaness.csv")

DF <- rbind(FairIsle, Hermaness)
DF$year = round(DF$year)

write.csv(DF, "data/length_Shetland.csv", row.names = FALSE)