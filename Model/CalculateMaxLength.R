
# Calculate theoretical max length, as defined by how long fish would become after one growth season if there was infinite food

CalculateMaxLengh <- function(iyear, NoDays, LENGTH) {

    i_max <- 1 / handling_time
    MaxLength <- 0

    for (iday in 1:NoDays){

        h_feed <- DayLengths[iday + NoDays * (iyear - 1)]

        A_max <- assimilation * h_feed * i_max

        MaxLength <- MaxLength + k * A_max
        
    }
    MaxLength <- MaxLength/1000 + LENGTH
    
    return(MaxLength)
}