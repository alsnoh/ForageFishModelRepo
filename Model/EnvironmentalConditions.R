#~~~~~~~~~~~ LOAD ENVIRONMENTAL DATA ~~~~~~~~~~#

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
prey_name <- prey_info$taxa


### visual parameters ### 
z = CONSTANTS$value[CONSTANTS$Parameter == "z"] # depth

  
  # latitude for day lengths
  #latitude = locations$centre_lat[locations$loc == scenario]
  #longitude = locations$centre_long[locations$loc == scenario]
  latitude = locations$centre_lat[locations$loc == "ECG"]
  longitude = locations$centre_long[locations$loc == "ECG"]
  
  # temperature 
  tempCSV <- temp_all[temp_all$location == scenario,]
  #tempCSV = read.csv(paste0("data/tempData/temp_", scenario, ".csv"), sep = ",")
  #tempCSV = read.csv(paste0("data/tempData/temp_ECG.csv"), sep = ",")
  temp <- tempCSV$temp[tempCSV$doy >= JD_ADDED & tempCSV$doy<= JD_FINISH]
  #temp = tempCSV$temp[yday(tempCSV$date) >= JD_ADDED & yday(tempCSV$date)<= JD_FINISH]
  tempAvg <- data.frame(temp = temp, year = tempCSV$year[tempCSV$doy >= JD_ADDED & tempCSV$doy<= JD_FINISH])
  #tempAvg <- data.frame(temp = temp, year = year(tempCSV$date[yday(tempCSV$date) >= JD_ADDED & yday(tempCSV$date)<= JD_FINISH]))
  tempAvg <- aggregate(temp ~ year, data = tempAvg, FUN = mean)
  # constant temp
  tempConst <- rep(mean(temp, na.rm = TRUE), length(temp)) 

  #years <- year(tempCSV$date[yday(tempCSV$date) >= JD_ADDED & yday(tempCSV$date)<= JD_FINISH])
  years <- tempCSV$year[tempCSV$doy >= JD_ADDED & tempCSV$doy<= JD_FINISH]
  #years <- 2003
  # prey abundance data for length of model run
  prey_abundance <- prey_abundance_all[prey_abundance_all$location == scenario,]
  prey_abundance$x <- NULL
  prey_abundance$y <- NULL
  #prey_abundance = read.csv(paste0("data/abundanceData/abundance_", scenario, ".csv"))
  #prey_abundance <- read.csv("data/abundanceData/abundance_NS_500.csv")
  #prey_abundance <- prey_abundance[prey_abundance$year >= tempCSV$year[1] & prey_abundance$year <= tempCSV$year[nrow(tempCSV)],]
  prey_abundance = prey_abundance[prey_abundance$jd >= JD_ADDED & prey_abundance$jd <= JD_FINISH,]
  #prey_abundance <- prey_abundance[prey_abundance$year %in% unique(years),]
  #years <- prey_abundance$year
  
 
  
  prey_abundanceConst <- prey_abundance[,4:ncol(prey_abundance)]
  prey_abundanceConst <- matrix(colMeans(prey_abundanceConst, na.rm = TRUE))
  ones <- matrix(rep(1, nrow(prey_abundance)), ncol = 1)
  prey_abundanceConst <- t(tcrossprod(prey_abundanceConst, ones))
  #prey_abundanceConst <- mutate(data.frame(prey_abundanceConst), obs = prey_abundance[,1], year = years, jd = prey_abundance$jd, .before = 1)
  prey_abundanceConst <- mutate(data.frame(prey_abundanceConst), obs = prey_abundance[,1], jd = prey_abundance$jd, .before = 1)
  

  # input id (if several scenarios are run in sequence)
  input_id <- 1:sum(tempCSV$doy == JD_ADDED)
    
  # day lengths
  DayLengths = rep( round(daylength(latitude, JD_ADDED:JD_FINISH)), max(input_id))
  DayLengthsConst <- floor(rep(mean(DayLengths), length(DayLengths)))
  
  # julian day vector
  JulianDayV = rep(JD_ADDED:JD_FINISH, max(input_id) )
  
  # light
  lightCSV = read.csv(paste0("data/light/light_ECG.csv"), sep = ",")
  #lightCSV = read.csv(paste0("data/light/light_", scenario, ".csv"), sep = ",")
  light = lightCSV$light[yday(lightCSV$date) >= JD_ADDED & yday(lightCSV$date)<= JD_FINISH]
  lightAvg <- data.frame(light = light, year = year(lightCSV$date[yday(lightCSV$date) >= JD_ADDED & yday(lightCSV$date)<= JD_FINISH]))
  lightAvg <- aggregate(light ~ year, data = lightAvg, FUN = mean)
  lightConst <- rep(mean(light, na.rm = TRUE), length(light))
  
  ac = 0.1 # diffuse attenuation coefficient
  #ambient_mult = exp(-ac*z)
  #light = light*ambient_mult
  a_c = rep(ac, length(JulianDayV ))
 
  ModelRunLengths <- rep(length(min(JulianDayV):max(JulianDayV)), max(input_id))
  NoModes <- length(unique(prey_mode))
  