
## write header file

write.table(paste0(
                   CONSTANTS$Parameter,
                   "=",
                   CONSTANTS$value
                   ),
            file ="Model/CONSTANTS.R", 
            quote=FALSE , sep = ",", row.names = FALSE, col.names = FALSE)






