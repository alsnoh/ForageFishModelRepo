
switch(exp_mode,
       write.csv(DF, paste0("Results/Atlantic/", scenario, ".csv"), row.names = F),
       write.csv(DF, paste0("Results/", scenario, ".csv"), row.names = F)
      )