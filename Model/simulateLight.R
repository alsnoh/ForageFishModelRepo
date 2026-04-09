
# function to simulate light levels over 24h based on a parabolic function with maximum at midday and minimum at midnight, scaled to the average light level for the day (light)
simulateLight <- function(light) {
    
    a <- -24*light/2304
    b <- -24 * a

    light_sim <- a * (1:24)^2 + b * (1:24)
    return(light_sim)

}