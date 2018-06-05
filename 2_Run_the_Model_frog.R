dat <- read.csv("year15totna.csv")[,-1]

jags.data <- list( x = dat,
                   nSites = nrow(dat),
                   nSurveys = ncol(dat))

Ninit <- apply(dat, 1, max, na.rm=TRUE)


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
