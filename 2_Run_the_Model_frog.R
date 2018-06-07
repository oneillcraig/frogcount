dat <- read.csv("goldfinch.csv")[,-1]

jags.data <- list( x = dat,
                   nSites = nrow(dat),
                   nSurveys = ncol(dat))

Ninit <- apply(dat, 1, max, na.rm=TRUE)
#I have some sites which were not sampled as often as others, so there are NA values at those surveys.  Added the "na.rm=TRUE" to account for this so that the Max value refers to the max value and not the NA.

inits <- function(){list(lambda = runif(1, 0, 10),
                         p = runif(1, 0, 1),
                         N = Ninit
)}



#------- Parameters monitored

params <- c("lambda", 
            "p")

#------- MCMC settings
ni <- 10000
nb <- 1000
nt <- 100
nc <- 3
na <- 1000

#------ call Library
library("jagsUI")

#------- Call JAGS from R

out <- jags(data = jags.data, inits = inits, parameters.to.save = params, model.file = "model.txt", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, n.adapt = na, parallel = TRUE)

print(out, dig = 3)

plot(out)
