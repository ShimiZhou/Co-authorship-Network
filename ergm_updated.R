edgelist_1990 = read.csv("/Users/zhoushimi/Desktop/co_authors1990_2000.csv")
library("igraph")
edgelist_1990 <- as.matrix(edgelist_1990)
coauthors_1990Network <- graph.edgelist(edgelist_1990, # the network object
                                        directed = FALSE # specify whether the network is directed
) 
coauthors_1990Network

library(statnet)
n1990 <- coauthors_1990Network
n1990
summary(n1990)
# Load in the attributes (gender) again
gender <- read.csv("/Users/zhoushimi/Desktop/gender_1990!.csv", stringsAsFactors = F)
# load in the co-authorship edge list again
edgelist_1990 = read.csv("/Users/zhoushimi/Desktop/co_authors1990_2000.csv")
# put them both in the network
n1990 <-graph_from_data_frame(edgelist_1990, directed = F, vertices = gender)
# look at the network
n1990
summary(n1990)

graph.density(n1990)

library(statnet)
library(intergraph)
# !!! do not forget to convert it to a statnet object
n1990 <- asNetwork(n1990)
# now n1990 is an object of class network
summary(n1990)
# Network attributes:
#   vertices = 1289
# directed = FALSE
# hyper = FALSE
# loops = TRUE
# multiple = TRUE
# bipartite = FALSE
# total edges = 1622
# missing edges = 0
# non-missing edges = 1622
# density = 0.001950914
# 
# Vertex attributes:
# 
#   Gender:
#   character valued attribute
# attribute summary:
#   Female   Male
# 246    936
# vertex.names:
#   character valued attribute
# 1289 valid vertex names
# 
# X:
#   logical valued attribute
# attribute summary:
#   Mode    NA's
# logical    1289
# 
# No edge attributes

# display the mixing matrix by gender
mixingmatrix(n1990, "Gender")
##            Female Male <NA>
##  Female     65  426   28
##  Male      426  960  128
##  <NA>       28  128   15

# Female Male
# Female     65  455
# Male      455 1102

# see a vector of attribute values 
gd <- n1990 %v% "Gender"
table(gd)
## Female   Male 
## 246    936 

# Female   Male 
# 247   1042 

# Fitting an ERG model
model1 <- ergm(n1990 ~ edges)
summary(model1)
# Call:
#   ergm(formula = n1990 ~ edges)
# 
# Maximum Likelihood Results:
#   
#         Estimate Std. Error MCMC % z value Pr(>|z|)    
# edges -6.26824    0.02524      0  -248.4   <1e-04 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Null Deviance: 1152572  on 831405  degrees of freedom
# Residual Deviance:   22869  on 831404  degrees of freedom
# 
# AIC: 22871  BIC: 22882  (Smaller is better. MC Std. Err. = 0)

names(model1)

# coefficient and likelihood
coef(model1)
## edges 
## -6.268239 
model1$mle.lik
## 'log Lik.' -11434.43 (df=1)

# Triad formation
summary(n1990 ~ edges + triangle)
# [1] "Warning:  This network contains loops"
# edges triangle 
# 1573      753 

summary(gender)

model2 <- ergm(n1990 ~ edges + nodematch("Gender"))
summary(model2)
# Call:
#   ergm(formula = n1990 ~ edges + nodematch("Gender"))
# 
# Maximum Likelihood Results:
#   
#                     Estimate Std. Error MCMC %  z value Pr(>|z|)    
# edges            -6.36073    0.04750      0 -133.915   <1e-04 ***
#   nodematch.Gender  0.13135    0.05607      0    2.343   0.0191 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Null Deviance: 1152572  on 831405  degrees of freedom
# Residual Deviance:   22863  on 831403  degrees of freedom
# 
# AIC: 22867  BIC: 22891  (Smaller is better. MC Std. Err. = 0)

model2$mle.lik
# 'log Lik.' -11431.63 (df=2)

# goodness of fit
gof.model2 <- gof(model2, GOF=~model)

# Goodness-of-fit for model statistics 
# 
#                   obs  min    mean  max MC p-value
# edges            1573 1476 1568.62 1671       0.96
# nodematch.Gender 1129 1041 1122.45 1195       0.80

plot(gof.model2)


model3 <- ergm(n1990 ~ edges + triangle, seed = 99)



