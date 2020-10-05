library(dgaFast) # Re-implements library(dga)

# Number of lists and prior hyperparameter
p <- 5
data(graphs5) # Decomposable graphical models on 5 lists.
delta <- 0.5
Nmissing <- 1:300 # Reasonable range for the number of unobserved individuals.

# Counts corresponding to list inclusion patterns.
Y <- c(0,27,37,19,4,4,1,1,97,22,37,25,2,1,3,5,83,36,34,18,3,5,0,2,30,5,23,8,0,3,0,2)
Y <- array(Y, dim=c(2,2,2,2,2))
N <- sum(Y) + Nmissing

# Model-wise posterior probaiblities on the total population size.
# weights[i,j] is the posterior probability for j missing individuals under model graphs5[[j]].
weights <- bma.cr(Y,  Nmissing, delta, graphs5)

png("./figures/example.png", width=1200, height=800, res=200)
# Plot of the posterior distribution.
plotPosteriorN(weights, N)
dev.off()

htmlSummary("./figures/posteriorSummary/summaryTable", weights, N, nrows=5, graphs=graphs5)

latexSummary("./figures/posteriorSummary/summaryTable", weights, N, nrows=5, graphs=graphs5)
