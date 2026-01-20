# Script to solve implicit Aksnes and Utne equation for visual range D


deriv <- function(preyIA, 
                E, 
                Eb, 
                Ke,
                beam_attenuation,
                r) {
  
    #log both sides, rearrange and differentiate with respect to r
    logRHS <- log(0.23* preyIA*E ) 
    logLHS <- log( ((Ke+Eb)/Eb) * r * r * exp(beam_attenuation * r)  ) 
    equation <- logLHS-logRHS
    derivative <- beam_attenuation + 2/r 
  
    
    return (c(equation,derivative))#equation
    #derivative
  
}



getr <- function(beam_attenuation, 
                preyIA, 
                E, 
                Eb, 
                Ke, 
                tol) {

    r <- sqrt(preyIA * E * (Eb/(Ke+Eb)) * 0.23)  #first guess for r
    
    vec <- deriv(preyIA, E, Eb, Ke, beam_attenuation, r)
    eq <- vec[1]
    der <- vec[2]

    while(abs(eq) > tol) {
        # Newtons method
        r <- r - eq/der

        vec <- deriv(preyIA, E, Eb, Ke, beam_attenuation, r)
        eq <- vec[1]
        der <- vec[2]
    }

    return (r)

}
