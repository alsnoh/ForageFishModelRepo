
if (max(DF$Weight) > 2.5) {
    write.csv(DF, paste0("Results/", scenario, "_test.csv"), row.names = F)
} else {
    write.csv(DF, paste0("Results/", scenario, ".csv"), row.names = F)
}
