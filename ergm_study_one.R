# --- preparing the network data: co-authors from 1990-2000 (follow the steps: https://bookdown.org/markhoff/social_network_analysis/your-first-network.html)
edgelist_1990 = read.csv("/Users/zhoushimi/Desktop/co_authors1990_2000.csv")
install.packages("igraph", repos='http://cran.us.r-project.org')
library("igraph")
edgelist_1990 <- as.matrix(edgelist_1990)
coauthors_1990Network <- graph.edgelist(edgelist_1990, # the network object
                                        directed = FALSE # specify whether the network is directed
                                        ) 
coauthors_1990Network
plot(coauthors_1990Network)

# --- doing analysis with Exponential Random Graph Model: 
# ERGMs imagine the observed network to be just one realization of a set of possible networks with similar features
# As the outcome of a stochastic process, it can control for other network factors, such as transitivity

# load ergm package
library(ergm)
# version: 4.4.0, updated in 2023-01-27: https://cran.r-project.org/web/packages/ergm/vignettes/ergm.pdf
packageVersion("ergm")
# "statnet" package that allows one to fit ERGM models is part of the statnet (statistical networks) suite of packages
# it includes a number of complementary packages for the statistical analysis of networks
library(statnet)
# convert igraph objects to statnet and vise versa - intergraph
install.packages("intergraph")
library(intergraph)
# use the asNetwork function to convert igraph objects to statnet objects
# Network attributes: vertices = 1289; undirected; total edges = 1622; no edge attributes
coauthors_1990Network <- asNetwork(coauthors_1990Network)
coauthors_1990Network
# set the system seed, which will allow us to repliate the results
set.seed(1234)
?ergm.terms
# count how many edges in the network
random_graph <- ergm(coauthors_1990Network ~ edges, control = control.ergm(seed = 1234))
# convert log-odds to probability to understand the coefficient: edges = 0.001891978 
inv.logit <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

theta <- coef(random_graph)
inv.logit(theta)

network.density(coauthors_1990Network)
# examine the model fit using the summary() function
summary(random_graph)
# - Result
## Call:
  ##ergm(formula = coauthors_1990Network ~ edges, control = control.ergm(seed = 1234)) 
  ## Maximum Likelihood Results:
  ##
  ## Estimate Std. Error MCMC % z value Pr(>|z|)    
  ## edges -6.26824    0.02524      0  -248.4   <1e-04 ***
  ## ---
  ## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  ## Null Deviance: 1152572  on 831405  degrees of freedom
  ## Residual Deviance:   22869  on 831404  degrees of freedom
  ##
  ## AIC: 22871  BIC: 22882  (Smaller is better. MC Std. Err. = 0)

# simulate graphs using ERGM fit
set.seed(1234)
hundred_simulations <- simulate(random_graph,
                                coef = theta,
                                nsim = 100,
                                control = control.simulate.ergm(MCMC.burnin = 1000,
                                                                MCMC.interval = 1000)
  
)
# examine the first nine simulations
par(mfrow = c(3,3))
sapply(hundred_simulations[1:9], plot, vertex.cex = 1, vertex.col = 'tomato')
# compare the number of edges our observed graph has to the average of the simulated networks
net_densities <- unlist(lapply(hundred_simulations, network.density))
hist(net_densities, xlab = "Density", main = "", col = "lightgray")
abline(v = network.density(coauthors_1990Network), col = "red", lwd = 3, lty = 2)
abline(v = mean(net_densities), col = "blue", lwd = 3, lty = 2)
# use the built-in goodness of fit measures:
# evaluate whether our network has similar structural features as the simulated graphs
gof_stats <- gof(random_graph)
par(mfrow = c(2,3))
plot(gof_stats, main = '')
# look at the g(y) stats for this model
## edges  triangle
##  1573       754
summary(coauthors_1990Network ~ edges + triangle)



