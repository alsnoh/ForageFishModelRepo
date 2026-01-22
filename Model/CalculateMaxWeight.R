
# Calculate theoretical max weight, as defined by how heavy fish would become after one growth season if there was infinite food

CalculateMaxWeight <- function(iyear, NoDays, assimilationV, WEIGHT) {

    i_max <- 1 / handling_time
    MaxWEIGHT <- 0

    for (iday in 1:NoDays){

        h_feed <- DayLengths[iday + NoDays * (iyear - 1)]
        assimilation <- assimilationV[iday + NoDays * (iyear - 1)]

        A_max <- assimilation * h_feed * i_max

        MaxWEIGHT <- MaxWEIGHT + k * A_max
        
    }
    MaxWEIGHT <- MaxWEIGHT/1000 + WEIGHT
    
    return(MaxWEIGHT)
}