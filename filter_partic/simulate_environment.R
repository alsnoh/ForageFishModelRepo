#~~~~~~~~ PREY CHARACTERISTICS ~~~~~~~~#


NoTaxa = 1


prey_size = rep(2.7, NoTaxa) # 2.7
prey_energy = rep(2.816, NoTaxa)
prey_ed = rep(4400, NoTaxa) # J/g
prey_mode = rep(1, NoTaxa) #prey_mode = as.numeric(prey_info$mode)
prey_image_area = rep(1.61, NoTaxa) #1.61
prey_name <- rep("CF", NoTaxa)

z = CONSTANTS$value[CONSTANTS$Parameter == "z"] # depth

# prey_abundance = matrix(nrow = 1, ncol = NoTaxa+3)
# for (i in 1:(NoTaxa)) {
#   prey_abundance[1,i+3] = 
# }

temp <- c(10)

DayLengths <- c(16)

JulianDayV <- c(1)

# light <- c(300)

ac = 0.1 # diffuse attenuation coefficient
a_c = rep(ac, length(JulianDayV ))

NoModes <- 1
ModelRunLengths <- c(100)