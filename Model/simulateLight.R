
# function to simulate light levels over 24h based on a parabolic function with maximum at midday and minimum at midnight, scaled to the average light level for the day (light)
simulateLight <- function(light, dl) {
    light_sim <- numeric(24)

    a <- -6 * light / dl^2
    b <- -dl * a
    for (i in 1:24) {
        if(i<5 || i > 5 + dl) {
            light_sim[i] <- 0
        } else {
            light_sim[i] <- a * (i-5)^2 + b * (i-5)
        }
    }
    return(light_sim)

}