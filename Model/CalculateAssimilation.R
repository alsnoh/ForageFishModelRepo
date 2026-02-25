# Construct and solve van bertalanffy growth equation with ingestion term
source("Model/getr.R")
CalculateAssimilation <- function(  iyear, 
                                    NoDays, 
                                    MaxWEIGHT, 
                                    MaxLENGTH, 
                                    assimilationV,
                                    prey_abundance, 
                                    prey_size, 
                                    prey_energy, 
                                    prey_ed, 
                                    prey_mode, 
                                    prey_image_area, 
                                    JulianDayV, 
                                    DayLengths, 
                                    light, 
                                    a_c,
                                    mu,
                                    lambda) {

    i_dailys <- numeric(NoDays)
    A_dailys <- numeric(NoDays)
    WEIGHT_daily <- numeric(NoDays)
    LENGTH_daily <- numeric(NoDays)
    h_feeds <- numeric(NoDays)

    for (iday in 1:NoDays){

        JulianDay <- JulianDayV[iday]
        #h_feed_max <- DayLengths[iday + NoDays * (iyear - 1)]
        h_feed_max <- DayLengths[iday + NoDays * (iyear - 1)] # for controlled experiments
        assimilation <- assimilationV[iday + NoDays * (iyear - 1)]

        feeding_time_fraction <- (MaxLENGTH-LENGTH)/MaxLENGTH # fraction of max length determines time spent feeding
        if(feeding_time_fraction < 0) {
            feeding_time_fraction <- 0
        }

        h_feed <- floor(h_feed_max * feeding_time_fraction) # hours spent feeding
        #h_feed <- h_feed_max # hours spent feeding

        #initialise numerator of functional response for each prey class (mode) to be summed
        func_response_numerator <- numeric(NoModes)
        denominator <- numeric(NoModes)


        for (itaxa in 1:NoTaxa){

            ac = a_c[iday]
            E <- (   ( (LENGTH / 100 )*dec_dist_scale)^2 )/(  C *  (  (  10^(2.62 *log10( 7 ) -2.01)   )/1000000   )   ) # eye sensitivity 
            ab <- (ac - 0.04)/0.2; # beam attenuation
            
            #script for solving implicit detection distance equation
            detection_distance <- getr(ab, 
                                    (prey_image_area[itaxa]/1000000),
                                    E,
                                    light[iday + NoDays * (iyear - 1)], # lightConst for controlled experiments, light for actual data
                                    kR,
                                    0.001)

            search_rate <- pi*(detection_distance^2)*swimming_speed*60*60 * ( (LENGTH )/100 ) 

            efficiency <- 1*(1-(1/(1+exp(-b* (log(prey_size[itaxa] /10.0 ) -  m  )  )))) # ok but decline in DB not so clear
            abundance <- prey_abundance[iday + NoDays * (iyear - 1), itaxa + 3]; # abundance of prey type on given day use  prey_abundanceConst for controlled experiments
            capture_rate <- efficiency * search_rate * abundance # capture rate ignoring handling time



            for(imode in 1:NoModes){ # adding on to respective numerators/denominators if type matches mode

                func_response_numerator[imode] <- func_response_numerator[imode] + capture_rate * prey_energy[itaxa]/prey_ed[itaxa] * (prey_mode[itaxa]==imode)
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

        # calculating maximum ingested weight per hour (assuming the sandeels spend time in each mode in proportion to the profitability of each mode)
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

            A_daily <- i_daily*assimilation #account for assimilation efficiency
        }

        i_dailys[iday] <- i_daily
        A_dailys[iday] <- A_daily

        h_feeds[iday] <- h_feed

        WEIGHT_daily[iday] <- WEIGHT
        LENGTH_daily[iday] <- LENGTH

        LENGTHcoeff <- LENGTH^(1-a2)/(a1*a2)

        # V1 growth based on von bertalanffy with ingestion term and asymptote at max weight - can be switched on/off by commenting out the relevant lines
        #  WEIGHT <- k * A_dailys[iday] * (MaxWEIGHT - WEIGHT) + WEIGHT
        #  LENGTH <- k * LENGTHcoeff * A_dailys[iday] * (MaxWEIGHT - a1*LENGTH^a2) + LENGTH

        # V2 growth based on von bertalanffy with ingestion term but no asymptote at max weight - can be switched on/off by commenting out the relevant lines
         WEIGHT <- k * A_dailys[iday] + WEIGHT
         LENGTH <- k * LENGTHcoeff * A_dailys[iday] + LENGTH

        # Model V3
        #WEIGHT <- A_dailys[iday] - mu * WEIGHT + WEIGHT
        #LENGTH <- (1/a2) * (A_dailys[iday] * (1/(a1*LENGTH^(a2-1))) - mu * LENGTH) + LENGTH 

        # Model V4 Doesnt work at all lol
        # WEIGHT <- A_dailys[iday] - mu * WEIGHT + WEIGHT
        # LENGTH <- k * (A_dailys[iday] * LENGTH^(1-a2) * MaxLENGTH - LENGTH) + LENGTH

        # Model V5 
        #WEIGHT <- lambda * A_dailys[iday] * WEIGHT^(2/3) - mu * WEIGHT + WEIGHT
        #LENGTH <- k * (A_dailys[iday] * MaxLENGTH - LENGTH) + LENGTH
    }
    results_DF <- data.frame(assimilated_weight = A_dailys, ingested_weight = i_dailys, weight = WEIGHT_daily, length = LENGTH_daily, jd = JulianDayV[1:length(WEIGHT_daily)], feeding_hours = h_feeds)
    
    return(results_DF)
}