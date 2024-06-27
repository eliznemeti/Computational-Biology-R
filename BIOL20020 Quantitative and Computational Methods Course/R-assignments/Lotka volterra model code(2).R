##clear R's memory
rm(list=ls())

## load in the packages you need 
## deSolve package for solving differential equations
library(deSolve)

## define a two species LV model
lv2 <- function(t, start, parms) {
  ##set the start values for each of the species
  ##basal species
  N <- start["N"]
  ## consumers
  P <- start["P"]
  
  ##allow R to look within parms for the values of r, a, e, etc
  with(as.list(parms), {
    ## dynamics of the resources
    dN <- r*N - c1*N*P
    ## dynamics of the primary consumers
    dP <- c2*P*N - d2*P
    ##return a list of the abundances of each species
    ##deSolve will then group these into a data frame
    list(c(dN, dP))
  })
}

##make an object of the parameters of the model
##we will pass this to the ODE solver
parms <- c(r = 0.2,         #growth rate of resources (r1 in the online model)
           c1 = 0.2,        #feeding rate of primary consumer on resources. (C1 in the online model)
           c2 = 0.2,        #efficinecy of feeding on resources. (C2 in the online model)
           d2 = 0.2          #intrinsic death rate of the primary consumer (d2 in the online model)
)

#define the starting numbers (not available on the online model)
start <- c(N = 100, 
           P = 30)

##set the length of the simulation (100 time steps)
## and the resolution (set size, in this case 0.1 time steps)
time <- seq(0, 100, 0.1)


##run the simulation
simulation_output <- as.data.frame(lsoda(y = start, 
                                        times = time, 
                                        func = lv2, 
                                        parms = parms))

##allow two plots side by side
par(mfrow=c(1,2)) 

##plot out the prey
plot(simulation_output$time, 
     simulation_output$N, 
     type="l",
     xlab="Time",
     ylab="Abundance")

##add the predators
points(simulation_output$time, 
       simulation_output$P, 
       type="l",
       col="red")

legend("topleft", 
       legend=c("Hare", "Lynx"),
       col=c("black", "red"), 
       lty=1, cex=0.5)

## make the phase plot
plot(simulation_output$P, 
     simulation_output$N, 
     type="l",
     xlab="Predator abundance", 
     ylab="Prey abundance")
