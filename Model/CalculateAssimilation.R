# Construct and solve van bertalanffy growth equation with ingestion term

CalculateAssimilation <- function(iyear, NoDays, i_dailys, LENGTH_daily, MaxLength) {

    for (iday in 1:NoDays){

        JulianDay <- JulianDayV[iday]
        h_feed <- DayLengths[iday + NoDays * (iyear - 1)]

        #initialise numerator of functional response for each prey class (mode) to be summed
        func_response_numerator <- numeric(NoModes)
        denominator <- numeric(NoModes)


        for (itaxa in 1:NoTaxa){

            E <- (   ( (LENGTH / 100 )*dec_dist_scale)^2 )/(  C *  (  (  10^(2.62 *log10( 7 ) -2.01)   )/1000000   )   ) # eye sensitivity 
            ab <- (ac - 0.04)/0.2; # beam attenuation
            
            #script for solving implicit detection distance equation
            detection_distance <- getr(ab, 
                                    (prey_image_area[itaxa]/1000000),
                                    E,
                                    light[iday + NoDays * (iyear - 1)], 
                                        kR,
                                        0.001)

            search_rate <- pi*(detection_distance^2)*swimming_speed*60*60 * ( (LENGTH )/100 ) 

            efficiency <- 1*(1-(1/(1+exp(-b* (log(prey_size[itaxa] /10.0 ) -  m  )  )))) # ok but decline in DB not so clear
            abundance <- prey_abundance[iday + NoDays * (iyear - 1), itaxa + 3]; # abundance of prey type on given day
            capture_rate <- efficiency * search_rate * abundance # capture rate ignoring handling time



            for(imode in 1:NoModes){ # adding on to respective numerators/denominators if type matches mode

                func_response_numerator[imode] <- func_response_numerator[imode] + capture_rate * prey_energy[itaxa] * (prey_mode[itaxa]==imode)
                denominator[imode] <- denominator[imode] + capture_rate * handling_time * (prey_mode[itaxa]==imode)
            }

        }

        intake_per_mode <- numeric(NoModes)
        total_max <- 0

        for (imode in 1:NoModes){
            intake_per_mode[imode] <- func_response_numerator[imode]/(1+denominator[imode])
            total_max <- total_max + intake_per_mode[imode]  # this is across modes - used for determining relative profitability

            denominator[imode] <- 0
            func_response_numerator[imode] <- 0
        }

        i_daily <- 0

        # calculating maximum ingested energy per hour (assuming the sandeels spend time in each mode in proportion to the profitability of each mode)
        if(total_max != 0) {
            i_hourly <- 0
            for (imode in 1:NoModes){
                    i_hourly <- i_hourly + (intake_per_mode[imode]/total_max)  *  intake_per_mode[imode]
            }

            # loop through all hours of feeding - update stomach content each hour
            for(h in 1:h_feed)  
            {
                i_daily <- i_daily + i_hourly
            }

            i_daily <- i_daily*assimilation #account for assimilation efficiency
            i_daily <- i_daily/1000 # put into kJ
        }

        i_dailys[iday] <- i_daily

        LENGTH_daily[iday] <- LENGTH
        LENGTH <- k * i_dailys[iday] * (MaxLength - LENGTH) + LENGTH
        
        
    }
    results_DF <- data.frame(assimilated_energy = i_dailys, length = LENGTH_daily, jd = JulianDayV)
    return(results_DF)
}