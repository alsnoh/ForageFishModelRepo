
#######################################################################################################################################################
######################################## Calculate assimilation and metabolism for daily growth of forage fish ########################################
#######################################################################################################################################################


source("Model/getr.R")
source("Model/simulateLight.R")
CalculateAssimilation <- function(  iyear, 
                                    NoDays, 
                                    MaxWEIGHT, 
                                    MaxLENGTH,
                                    temp, 
                                    #assimilationV,
                                    prey_abundance, 
                                    prey_size, 
                                    prey_energy, 
                                    prey_ed, 
                                    prey_mode, 
                                    prey_image_area,
                                    prey_name, 
                                    JulianDayV, 
                                    DayLengths, 
                                    light, 
                                    a_c,
                                    mu,
                                    lambda,
                                    LENGTH,
                                    WEIGHT,
                                    ENERGY,
                                    z) 
{

    i_dailys <- numeric(NoDays)
    A_dailys <- numeric(NoDays)
    M_dailys <- numeric(NoDays)
    WEIGHT_daily <- numeric(NoDays)
    LENGTH_daily <- numeric(NoDays)
    ENERGY_daily <- numeric(NoDays)
    h_feeds <- numeric(NoDays)
    search_rates <- numeric(NoDays)
    particulates <- numeric(NoDays)
    filters <- numeric(NoDays)
    depths <- numeric(24*NoDays)

    # loop through days in growth season
    for (iday in 1:NoDays)
    {

        # Keep track of Julian Day for model output
        JulianDay <- JulianDayV[iday]

        # Calculate factors that update each day not hour (temp data is daily)
        h_feed_max <- DayLengths[iday + NoDays * (iyear - 1)] # hours of daylight
        #h_feed_max <- 24 # for testing with constant day lengths
        assimilation <- (A1 + A2*temp[iday + NoDays * (iyear - 1)])-Ua # temp dependent assimilation efficiency

        metabolism <-  M_FEED*Q10_MF^(temp[iday + NoDays * (iyear - 1)] / 10) # temp dependent metabolic cost
        MET_SMR <- WEIGHT^rrr * metabolism # standard metabolic cost for 24h

        # calculate hours feeding based on length relative to max length (can be switched off by setting h_feed = h_feed_max)
        # assumes fish forage less as they approach max length
        feeding_time_fraction <- (MaxLENGTH-LENGTH)/MaxLENGTH 
        if(feeding_time_fraction < 0)
        {
             feeding_time_fraction <- 0
        }
        #h_feed <- floor(h_feed_max * feeding_time_fraction) # hours spent feeding
        #h_feed <- h_feed_max # hours spent feeding
        h_feed <- 24

        probability <- numeric(NoTaxa)
        abundance <- numeric(NoTaxa)
        handling_times <- numeric(NoTaxa)



        ################################### Filter submodel ##########################################

        # Initialize filter intake for one hour to be summed for each prey type. Holling type I model
        filter <- 0
        profitability_filter <- numeric(NoTaxa)
        # as things stand filter feeding doesn't vary by hour because of data resolution but this might change hence calculated hourly

        for (itaxa in 1:NoTaxa)
        {

            # probability of capturing prey type for particulate feeding (sigmoidal function of prey size) 
            # Filter probability calculated as a fraction of particulate efficiency (assumed to be less)
            probability[itaxa] <- 1*(1-(1/(1+exp(-b* (log(prey_size[itaxa] /10.0 ) -  m  )  )))) 
            filter_probability <- probability[itaxa] * 0.8


            abundance[itaxa] <- prey_abundance[iday + NoDays * (iyear - 1), itaxa + 3] # abundance of prey type on given day 
            retention_efficiency <- prey_size[itaxa] / (1 + prey_size[itaxa]) # efficiency of retaining prey once captured by filter feeding

            filter <- filter + filter_probability * retention_efficiency * prey_energy[itaxa] * abundance[itaxa] #/ prey_ed[itaxa] #for weight not energy

            # Measure of profitability of each prey type when filter feeding. To be used to diet optimality and analysis of submodels
            profitability_filter[itaxa] <- filter_probability * retention_efficiency * prey_energy[itaxa] 
        }

        # calculate gape size and multiply by filter speed (assumed to be slower than swimming speed per hour for particulate feeding),
        # constant efficiency currently set arbitrarily, and filtered food  
        gape_max <- Ag_frac * MaxLENGTH
        gape_size <- gape_max * LENGTH/(1+LENGTH) # gape size increases with length but asymptotes at gape_max

        i_filter <- filter_speed * 60 * 60 * gape_size * filter  # hourly filter feeding intake 





        ###################################### Particulate submodel ##########################################
        # Holling type II model

        # Light and turbidity (beam attenuation)
        # Either used daily avg light data or this crudely simulated diel light
        light_sim <- simulateLight(light[iday + NoDays * (iyear - 1)], h_feed_max) # lightConst for controlled experiments, light for actual data
        ac = a_c[iday]
        ab <- (ac - 0.04)/0.2; # beam attenuation

        # Fish eye sensitivity to prey contrast - function of eye size (which is a function of fish length) and prey image area 
        E <- (   ( (LENGTH / 100 )*dec_dist_scale)^2 )/(  C *  (  (  10^(2.62 *log10( 7 ) -2.01)   )/1000000   )   )

        #initialise numerator and denominator of functional response for each prey class (mode) to be summed
        func_response_numerator <- numeric(NoModes)
        denominator <- numeric(NoModes)

        for (itaxa in 1:NoTaxa)
        {
            handling_times[itaxa] <- handling_time + (5/3600) * prey_size[itaxa] / (LENGTH/10) # handling time increases with prey size and decreases with predator size
        }

        # loop to calculate hourly particulate feeding intake
        i_partic <- numeric(h_feed)
        profitability_partic <- numeric(NoTaxa)

        for (hour in 1:h_feed) 
        {   
            distances <- c()
            ambient_mult = exp(-ac*z)
            light_at_depth <- light_sim[hour] * ambient_mult

            depths[hour + (iday-1)*h_feed] <- z

            for (itaxa in 1:NoTaxa)
            {
            
                #script for solving implicit detection distance equation
                detection_distance <- getr(ab, 
                                        (prey_image_area[itaxa]/1000000),
                                        E,
                                        light_at_depth, # lightConst for controlled experiments, light for actual data
                                        kR,
                                        0.001) # tolerance
            

                search_rate <- pi*(detection_distance^2)*swimming_speed*60*60 * ( (LENGTH )/100 ) 
                capture_rate <- probability[itaxa] * search_rate * abundance[itaxa] # capture rate ignoring handling time
            

                # calcuate numerator and denominator of functional response for each prey mode (class)
                # This is later used in a weighted sum to calculate total intake across modes
                for(imode in 1:NoModes)
                { 
                    func_response_numerator[imode] <- func_response_numerator[imode] + capture_rate * prey_energy[itaxa] * (prey_mode[itaxa]==imode)# / prey_ed[itaxa] #for weight not energy
                    denominator[imode] <- denominator[imode] + capture_rate * handling_times[itaxa] * (prey_mode[itaxa]==imode)
                }

                # measure of profitability of each prey type for particulate feeding - used for diet optimality and analysis of submodels
                profitability_partic[itaxa] <- probability[itaxa] * prey_energy[itaxa] / handling_times[itaxa] 

                distances <- c(distances, detection_distance)
            }
            avgDIST <- mean(distances)
            z <- 19*a*light_at_depth*exp(-ac*z) / (swimming_speed * LENGTH^2) - a*avgDIST^2*pi*swimming_speed*LENGTH + z
            if (z < 0) {
                z <- 0
            }

            # Calculate functional response for reach mode as well as sum across modes
            intake_per_mode <- numeric(NoModes)
            total_max <- 0
            for (imode in 1:NoModes)
            {
                intake_per_mode[imode] <- func_response_numerator[imode]/(1+denominator[imode])
                total_max <- total_max + intake_per_mode[imode]  # this is across modes - used for determining relative profitability

                # reset numerator and denominator for next hour of feeding
                denominator[imode] <- 0
                func_response_numerator[imode] <- 0
            }

            # calculating maximum ingested weight per hour (assuming the fish spend time in each mode in proportion to the profitability of each mode)
            i_hourly <- 0
            if(total_max != 0) 
            {
                for (imode in 1:NoModes)
                {
                        i_hourly <- i_hourly + (intake_per_mode[imode]/total_max)  *  intake_per_mode[imode]
                }
            }

            # store hourly intake    
            i_partic[hour] <- i_hourly
        }


    ####################################### Combine submodels and calculate growth ###########################################

        # reset total daily intake each
        i_daily <- 0
        M_daily <- 0

        # loop through all hours of feeding
        for(h in 1:h_feed)  
        {   
            # fitness is calculated as intake minus metabolic cost for the hour
            fitness_partic <- i_partic[h] - MET_SMR/24
            fitness_filter <- i_filter - MET_SMR/24 
            #fitness_filter <- 0 # for testing without filter feeding

            # weighted average of particulate and filter feeding intake based on relative fitness
            i_daily <- i_daily + (fitness_partic * i_partic[h] + fitness_filter * i_filter) / (fitness_partic + fitness_filter)
                
            # # or just one or the other for each hour depending on which is higher ingestion
            # if (i_partic[h] > i_filter) {
            # i_daily <- i_daily + i_partic[h]
            # } else {
            # i_daily <- i_daily + i_filter
            # }
        }

        # convert to kJ and calculate assimilated energy by multiplying by assimilation efficiency
        i_daily <- i_daily / 1000 
        A_daily <- i_daily * assimilation
            
        M_daily <- (fitness_partic * MET_SMR * exp(swimming_speed*LENGTH * 0.02) + fitness_filter * MET_SMR * exp(filter_speed*LENGTH * 0.02)) / (fitness_partic + fitness_filter)

        # store daily values for analysis and plotting
        particulates[iday] <- sum(i_partic)
        filters[iday] <- i_filter * h_feed
        i_dailys[iday] <- i_daily
        A_dailys[iday] <- A_daily
        search_rates[iday] <- pi*(avgDIST^2)*swimming_speed*60*60 * ( (LENGTH )/100 )
        h_feeds[iday] <- h_feed_max
        M_dailys[iday] <- M_daily

        ENERGY_daily[iday] <- ENERGY
        WEIGHT_daily[iday] <- WEIGHT
        LENGTH_daily[iday] <- LENGTH

        # calculate new values
        # V6 with energy instead and explicit metabolism
        ENERGY <- k * (A_dailys[iday]) - M_dailys[iday] + ENERGY
        #ENERGY <- k * (A_dailys[iday]) - MET_SMR * exp(swimming_speed*LENGTH * 0.02) + ENERGY
        WEIGHT <- ENERGY / ED
        LENGTH <- (WEIGHT/a1)^(1/a2)

        
    }

    # arrange profitabilities in descending order for plotting and analysis of diet optimality
    profitability_filter <- arrange(data.frame(profitability = profitability_filter, taxa = prey_name), by = desc(profitability))
    profitability_partic <- arrange(data.frame(profitability = profitability_partic, taxa = prey_name), by = desc(profitability))

    # store results for the year in a dataframe to be returned to main model loop
    results_DF <- data.frame(assimilated_weight = A_dailys, ingested_weight = i_dailys, weight = WEIGHT_daily, length = LENGTH_daily, jd = JulianDayV[1:length(WEIGHT_daily)], feeding_hours = h_feeds, search_rate = search_rates, particulates = particulates, filters = filters, metabolism = M_dailys)
    plot(-depths[1440:1488], type = "l")
    #plot(-depths, type = "l")
    return(results_DF)
}


################################ model graveyard #####################################################################
        # LENGTHcoeff <- LENGTH^(1-a2)/(a1*a2)
        # V1 growth based on von bertalanffy with ingestion term and asymptote at max weight - can be switched on/off by commenting out the relevant lines
        #  WEIGHT <- k * A_dailys[iday] * (MaxWEIGHT - WEIGHT) + WEIGHT
        #  LENGTH <- k * LENGTHcoeff * A_dailys[iday] * (MaxWEIGHT - a1*LENGTH^a2) + LENGTH

        # V2 growth based on von bertalanffy with ingestion term but no asymptote at max weight - can be switched on/off by commenting out the relevant lines
        # WEIGHT <- k * A_dailys[iday] + WEIGHT
         #LENGTH <- k * LENGTHcoeff * A_dailys[iday] + LENGTH

        # Model V3
        #WEIGHT <- A_dailys[iday] - mu * WEIGHT + WEIGHT
        #LENGTH <- (1/a2) * (A_dailys[iday] * (1/(a1*LENGTH^(a2-1))) - mu * LENGTH) + LENGTH 

        # Model V4 Doesnt work at all lol
        # WEIGHT <- A_dailys[iday] - mu * WEIGHT + WEIGHT
        # LENGTH <- k * (A_dailys[iday] * LENGTH^(1-a2) * MaxLENGTH - LENGTH) + LENGTH

        # Model V5 
        #WEIGHT <- lambda * A_dailys[iday] * WEIGHT^(2/3) - mu * WEIGHT + WEIGHT
        #LENGTH <- k * (A_dailys[iday] * MaxLENGTH - LENGTH) + LENGTH

        #V7 other form
        # n <- 3/4
        # WEIGHT <- A_dailys[iday] * (1 - (WEIGHT/MaxWEIGHT)^(1-n)) + WEIGHT
        # LENGTH <- (WEIGHT/a1)^(1/a2)