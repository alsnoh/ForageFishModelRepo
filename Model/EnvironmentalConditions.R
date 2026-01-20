#~~~~~~~~~~~ LOAD ENVIRONMENTAL DATA ~~~~~~~~~~#
# julian day of end of model run
JD_FINISH = 212

#~~~~~~~~ PREY CHARACTERISTICS ~~~~~~~~#
prey_info = read.csv("data/prey_info.csv")

NoTaxa = nrow(prey_info)

convert_to_number <- function(letters) {
  lapply(letters, function(letter) {
    utf8ToInt(tolower(letter)) - utf8ToInt("a") + 1
  })
}
prey_size = as.numeric(prey_info$size)
prey_energy = as.numeric(prey_info$energy)
prey_ed = prey_info$energy_density # J/g
prey_mode = convert_to_number(prey_info$mode) #prey_mode = as.numeric(prey_info$mode)
prey_image_area = as.numeric(prey_info$image_area)


### visual parameters ### 
z = CONSTANTS$value[CONSTANTS$Parameter == "z"] # depth


#  prey abundance data for length of model run
  prey_abundance = read.csv(paste0("data/abundanceData/abundance_", scenario, ".csv"))
  prey_abundance = prey_abundance[prey_abundance$jd >= JD_ADDED & prey_abundance$jd <= JD_FINISH,]
  #prey_abundance_year <- subset(prey_abundance, year == runYear)
  #prey_abundance <- prey_abundance_year
  years <- prey_abundance$year

  # input id (if several scenarios are run in sequence)
  input_id = rep(1:sum(prey_abundance$jd == JD_ADDED), each = length(JD_ADDED:JD_FINISH))
  
  
  # latitude for day lengths
  latitude = locations$centre_lat[locations$loc == scenario]
  longitude = locations$centre_long[locations$loc == scenario]
  
  # temperature 
  # temp = read.csv(paste0("data/tempData/temp_", scenario, ".csv"), sep = ",")
  # temp = temp$temp[yday(temp$date) >= JD_ADDED & yday(temp$date)<= JD_FINISH]
  # # constant temp
  # tempConst <- rep(1, length(temp)) 
    
  # day lengths
  DayLengths = rep( round(daylength(latitude, JD_ADDED:JD_FINISH)), max(input_id))
  
  # julian day vector
  JulianDayV = rep(JD_ADDED:JD_FINISH, max(input_id) )
  
  # light
  light = read.csv(paste0("data/light/light_", scenario, ".csv"), sep = ",")
  light = light$light[yday(light$date) >= JD_ADDED & yday(light$date)<= JD_FINISH]
  
  ac = 0.1 # diffuse attenuation coefficient
  ambient_mult = exp(-ac*z)
  light = light*ambient_mult
  a_c = rep(ac, length(JulianDayV ))

  
  
  JD_ADDED <- rep(JD_ADDED, max(input_id))  
  
  ModelRunLengths <- rep(length(min(JulianDayV):max(JulianDayV)), max(input_id))
  NoModes <- length(unique(prey_mode))
  