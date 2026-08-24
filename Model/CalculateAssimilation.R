
#######################################################################################################################################################
######################################## Calculate assimilation and metabolism for daily growth of forage fish ########################################
#######################################################################################################################################################


source("Model/getr.R")
source("Model/simulateLight.R")
CalculateAssimilation <- function(  iyear, 
                                    NoDays,
                                    temp,
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
                                    LENGTH,
                                    WEIGHT,
                                    ENERGY,
                                    WAM) 
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
    percentages_partic <- numeric(NoDays)
    percentages_filter <- numeric(NoDays)
    percentages_hiding <- numeric(NoDays)
    metabolisms <- numeric(NoDays)
    assimilations <- numeric(NoDays)
    gape_sizes <- numeric(NoDays)
    #depths <- numeric(24*NoDays)
    #depths_daily <- numeric(NoDays)

    # loop through days in growth season
    for (iday in 1:NoDays)
    {

        # Keep track of Julian Day for model output
        JulianDay <- JulianDayV[iday]

        # Calculate factors that update each day not hour (temp data is daily)
        daylength <- DayLengths[iday + NoDays * (iyear - 1)] # hours of daylight
        assimilation <- ((A1 + A2*temp[iday + NoDays * (iyear - 1)])-Ua) # temp dependent assimilation efficiency

        metabolism <-  M_FEED*Q10_MF^(temp[iday + NoDays * (iyear - 1)] / 10) # temp dependent metabolic cost
        MET_SMR <- WEIGHT^rrr * metabolism # standard metabolic cost for 24h

        h_feed <- 24

        # Light and turbidity (beam attenuation)
        # Either used daily avg light data or this crudely simulated diel light
        light_sim <- simulateLight(light[iday + NoDays * (iyear - 1)], daylength) 
        #light_sim <- numeric(h_feed) 
        #light_sim[1:daylength] <- light[iday + NoDays * (iyear - 1)] # for testing with no diel cycle just constant light during daylight hours
        ac = a_c[iday]
        ab <- (ac - 0.04)/0.2; # beam attenuation
        light_at_depth <- numeric(h_feed)


        probability <- numeric(NoTaxa)
        abundance <- numeric(NoTaxa)
        handling_times <- numeric(NoTaxa)



        ################################### Filter submodel ##########################################

        # Initialize filter intake for one hour to be summed for each prey type. Holling type I model
        filter <- 0
        profitability_filter <- numeric(NoTaxa)
        filter_speed <- numeric(h_feed)
        i_filter <- numeric(h_feed)
        # as things stand filter feeding doesn't vary by hour because of data resolution but this might change hence calculated hourly

        for (itaxa in 1:NoTaxa)
        {

            # probability of capturing prey type for particulate feeding (sigmoidal function of prey size) 
            # Filter probability calculated as a fraction of particulate efficiency (assumed to be less)
            probability[itaxa] <- 1*(1-(1/(1+exp(-b* (log(prey_size[itaxa] /10.0 ) -  m  )  )))) 
            filter_probability <- probability[itaxa] * filter_fraction


            #abundance[itaxa] <- prey_abundance[iday + NoDays * (iyear - 1), itaxa + 3] # abundance of prey type on given day
            abundance[itaxa] <- prey_abundance[iday, itaxa+1] 
            retention_efficiency <- prey_size[itaxa] / (1 + prey_size[itaxa]) # efficiency of retaining prey once captured by filter feeding

            filter <- filter + filter_probability * retention_efficiency * prey_energy[itaxa] * abundance[itaxa] #/ prey_ed[itaxa] #for weight not energy

            # Measure of profitability of each prey type when filter feeding. To be used to diet optimality and analysis of submodels
            profitability_filter[itaxa] <- filter_probability * retention_efficiency * prey_energy[itaxa] 
        }

        avgAbundance <- mean(abundance)

        # calculate gape size and multiply by filter speed (assumed to be slower than swimming speed per hour for particulate feeding),
        # constant efficiency currently set arbitrarily, and filtered food  
        gape_radius <- 0.5*Ag1*LENGTH#0.5*( 0.108883*LENGTH*10-0.0007684)#0.5*(Ag1*(LENGTH*10)^2 + Ag2*LENGTH*10) # Ag2*LENGTH*5  # -2.625+0.1731*(LENGTH)-0.0003099*(LENGTH*10)^2 Gape width as function of length for horse mackerel
        gape_size <- pi * (gape_radius * 0.001)^2 # convert to area m^2
        for (hour in 1:h_feed) 
        {

            ambient_mult = exp(-ac*z)
            light_at_depth[hour] <- light_sim[hour] * ambient_mult # * ambient_mult #light[1] #light_sim[hour] * ambient_mult 

            if (light_at_depth[hour] > 10)
            {
                filter_speed[hour] <- filter_speed_max
            } else if (light_at_depth[hour] < 0.1) {
               filter_speed[hour] <- filter_speed_max * 0.25
            } else {
               filter_speed[hour] <- filter_speed_max * (light_at_depth[hour] + 3.2) / 13.2
            }

            i_filter[hour] <- filter_speed[hour] * 60 * 60 * (LENGTH / 100) * gape_size * filter  # hourly filter feeding intake 
        }





        ###################################### Particulate submodel ##########################################
        # Holling type II model

        # Fish eye sensitivity to prey contrast - function of eye size (which is a function of fish length) and prey image area 
        E <- (   ( (LENGTH / 100 )*dec_dist_scale)^2 )/(  C *  (  (  10^(2.62 *log10( 7 ) -2.01)   )/1000000   )   )

        #initialise numerator and denominator of functional response for each prey class (mode) to be summed
        func_response_numerator <- numeric(NoModes)
        denominator <- numeric(NoModes)

        for (itaxa in 1:NoTaxa)
        {
            #handling_times[itaxa] <- handling_time + (5/3600) * prey_size[itaxa] / (LENGTH/10) # handling time increases with prey size and decreases with predator size
            handling_times[itaxa] <- handling_time # for testing with constant handling times
        }

        # loop to calculate hourly particulate feeding intake
        i_partic <- numeric(h_feed)
        dists <- numeric(h_feed)
        profitability_partic <- numeric(NoTaxa)

        for (hour in 1:h_feed) 
        {   
            distances <- numeric(NoTaxa)

            #depths[hour + (iday-1)*h_feed] <- z

            for (itaxa in 1:NoTaxa)
            {
            
                #script for solving implicit detection distance equation
                detection_distance <- getr(ab, 
                                        (prey_image_area[itaxa]/1000000),
                                        E,
                                        light_at_depth[hour], 
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

                distances[itaxa] <- detection_distance
            }
            avgDIST <- mean(distances)
            # z <- 19*a*light_at_depth*exp(-ac*z) / (swimming_speed * LENGTH^2) - 10*a*avgDIST^2*pi*swimming_speed*LENGTH + z

            # if (light_at_depth > I_opt) {
            #     z <- z + 1#0.01 * LENGTH * swimming_speed * 60 * 60 / 100
            # } else if (light_at_depth < I_opt) {
            #     z <- z - 1#0.01 *LENGTH * swimming_speed * 60 * 60 / 100
            # } else {
            #    z <- z
            # }

            # if (z < 0) {
            #     z <- 0
            # }

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
            dists[hour] <- avgDIST
        }


    ####################################### Combine submodels and calculate growth ###########################################
        #hoursEating <- h_feed 
        # reset total daily intake each
        A_daily <- numeric(h_feed)
        M_daily <- numeric(h_feed)
        fitness_partic <- numeric(h_feed)
        fitness_filter <- numeric(h_feed)
        fitness_hiding <- numeric(h_feed)
        net_energy <- numeric(h_feed)
        

        

        particMeta <- MET_SMR * exp(swimming_speed*LENGTH * nu)
        filterMeta <- fitness_met_mult*MET_SMR * exp(filter_speed*LENGTH * nu)

        # loop through all hours of feeding
        A_partic <- assimilation * i_partic
        A_filter <- assimilation * i_filter
        for(h in 1:h_feed)  
        {   
            # fitness is calculated as intake minus metabolic cost for the hour
            fitness_partic[h] <- (24 * (A_partic[h])) / (particMeta * (1+mu)) #max(A_partic[h] - particMeta/24, 0)
            fitness_filter[h] <- (24 * (A_filter[h])) / (filterMeta[h] * (1+mu)) #max(A_filter[h] - filterMeta[h]/24, 0)
            #fitness_hiding[h] <- 24 / MET_SMR #-MET_SMR/24 # fitness of hiding is negative metabolic cost with no intake

            #fitness_filter[h] <- 0 # for testing without filter feeding
            #fitness_partic[h] <- 0 # for testing without particulate feeding
        }
        fitnessFilterDF <- data.frame(hour = 1:h_feed, fitness_filter = fitness_filter, assimilation_filter = A_filter, meta_filter = filterMeta/24)
        fitnessParticDF <- data.frame(hour = 1:h_feed, fitness_partic = fitness_partic, assimilation_partic = A_partic, meta_partic = rep(particMeta/24, h_feed))

        fitnessFilterDF <- fitnessFilterDF %>% arrange(desc(fitness_filter))
        fitnessParticDF <- fitnessParticDF %>% arrange(desc(fitness_partic))

        count <- 0
        for(hh in 1:h_feed){

            if ((fitnessParticDF$fitness_partic[hh] + fitnessFilterDF$fitness_filter[hh]) <= 0) # dont eat if no prey
            {
                A_daily[hh] <- 0
                M_daily[hh] <- MET_SMR/24
            } else
            {
                A_daily[hh] <- (fitnessParticDF$fitness_partic[hh] * fitnessParticDF$assimilation_partic[hh] + fitnessFilterDF$fitness_filter[hh] * fitnessFilterDF$assimilation_filter[hh]) / (fitnessParticDF$fitness_partic[hh] + fitnessFilterDF$fitness_filter[hh])
                M_daily[hh] <- (fitnessParticDF$fitness_partic[hh] * fitnessParticDF$meta_partic[hh] + fitnessFilterDF$fitness_filter[hh] * fitnessFilterDF$meta_filter[hh]) / (fitnessParticDF$fitness_partic[hh] + fitnessFilterDF$fitness_filter[hh])

                #A_daily[hh] <- (fitness_partic[hh] * A_partic[hh] + fitness_filter[hh] * A_filter[hh] + fitness_hiding[hh] * 0) / (fitness_partic[hh] + fitness_filter[hh] + fitness_hiding[hh])
                #M_daily[hh] <- (fitness_partic[hh] * particMeta + fitness_filter[hh] * filterMeta[hh] + fitness_hiding[hh] * MET_SMR/24) / (fitness_partic[hh] + fitness_filter[hh] + fitness_hiding[hh])

            }
            A_daily[hh] <- A_daily[hh] / 1000 # convert to kJ

            net_energy[hh] <- A_daily[hh] - M_daily[hh]
            if (net_energy[hh] < -(MET_SMR/24)) # dont eat if energy gain is less than metabolic cost for the hour
            {
                A_daily[hh] <- 0
                M_daily[hh] <- MET_SMR/24
                count <- count + 1
            }
            
        }

        #M_daily[hoursEating+1] <- (24 - hoursEating) * MET_SMR / 24 # metabolic cost for hours not spent feeding

        total_fitness_filter <- sum(fitness_filter[1:h_feed])
        total_fitness_partic <- sum(fitness_partic[1:h_feed])
        total_fitness_hiding <- sum(fitness_hiding[1:h_feed])
        percentage_partic <- 100 * total_fitness_partic / (total_fitness_partic + total_fitness_filter + total_fitness_hiding)
        percentage_filter <- 100 * total_fitness_filter / (total_fitness_partic + total_fitness_filter + total_fitness_hiding)
        percentage_hiding <- 100 - percentage_partic-percentage_filter


        # convert to kJ and calculate assimilated energy by multiplying by assimilation efficiency
        #i_daily <- i_daily / 1000 
        i_daily <- A_daily / assimilation # back calculate ingested weight based on assimilated weight and assimilation efficiency
            
        
        #depths_daily[iday] <- mean(depths[1 + (iday-1)*h_feed:(iday*h_feed)]) 

        # store daily values for analysis and plotting
        particulates[iday] <- sum(i_partic)
        filters[iday] <- sum(i_filter)
        i_dailys[iday] <- sum(i_daily)
        A_dailys[iday] <- sum(A_daily)
        search_rates[iday] <- pi*(sum(dists)^2)*swimming_speed*60*60 * ( (LENGTH )/100 )
        h_feeds[iday] <- h_feed - count
        M_dailys[iday] <- sum(M_daily)
        percentages_partic[iday] <- percentage_partic
        percentages_filter[iday] <- percentage_filter
        percentages_hiding[iday] <- percentage_hiding
        metabolisms[iday] <- metabolism
        assimilations[iday] <- assimilation
        gape_sizes[iday] <- gape_size


        ENERGY_daily[iday] <- ENERGY
        WEIGHT_daily[iday] <- WEIGHT
        LENGTH_daily[iday] <- LENGTH

        # maturation function
        maturation <- 1 / (1+(WEIGHT/WAM)^(-5))
        reproduction <- repro_coeff * WEIGHT

        # calculate new values
        # V6 with energy instead and explicit metabolism
        #ENERGY <- k * (A_dailys[iday] - maturation * M_dailys[iday]) + ENERGY
         ENERGY <- k * (A_dailys[iday] - M_dailys[iday] - maturation * reproduction) + ENERGY
        #ENERGY <- k * (A_dailys[iday]) - MET_SMR * exp(swimming_speed*LENGTH * 0.02) + ENERGY
        WEIGHT <- ENERGY / ED
        LENGTH <- (WEIGHT/a1)^(1/a2)
        #print(LENGTH)

        if (ENERGY <= 0)
        {
            ENERGY <- 0
            WEIGHT <- 0
            LENGTH <- 0
            break
        }
        
    }
    if (ENERGY == 0)
    {
        results_DF <- data.frame(assimilated_weight = A_dailys, ingested_weight = i_dailys, weight = WEIGHT_daily, length = LENGTH_daily, energy = ENERGY_daily, jd = JulianDayV[1:length(WEIGHT_daily)], feeding_hours = h_feeds, search_rate = search_rates, particulates = particulates, filters = filters, metabolism = M_dailys, percentage_particulates = percentages_partic, percentage_filters = percentages_filter, percentage_hiding = percentages_hiding)
        return(results_DF)
    }

    # arrange profitabilities in descending order for plotting and analysis of diet optimality
    profitability_filter <- arrange(data.frame(profitability = profitability_filter, taxa = prey_name), by = desc(profitability))
    profitability_partic <- arrange(data.frame(profitability = profitability_partic, taxa = prey_name), by = desc(profitability))

    # store results for the year in a dataframe to be returned to main model loop
    results_DF <- data.frame(assimilated_weight = A_dailys, ingested_weight = i_dailys, weight = WEIGHT_daily, length = LENGTH_daily, energy = ENERGY_daily, jd = JulianDayV[1:length(WEIGHT_daily)], feeding_hours = h_feeds, search_rate = search_rates, particulates = particulates, filters = filters, metabolism = M_dailys, percentage_particulates = percentages_partic, percentage_filters = percentages_filter, percentage_hiding = percentages_hiding, metaConst = metabolisms, assimilation = assimilations, light = light[1], preyAbundance = avgAbundance, gape_size = gape_sizes)
    #plot(-depths[1440:1488], type = "l")
    #plot(-depths, type = "l")
    #plot(depths_daily, type = "l")
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