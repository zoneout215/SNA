install.packages('ergm')
install.packages('coda')
install.packages('rgl')


suppressPackageStartupMessages(library(ergm))
suppressPackageStartupMessages(library(coda))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(rgl))
suppressPackageStartupMessages(library(sna))


data(florentine, package='ergm')
# Of course, let's look at data first.
# We've seen this image in 3-D a couple of weeks ago:
col20 <- c(brewer.pal(11,'RdYlBu'),brewer.pal(9,'RdYlBu')) 
par(mar=c(1,1,1,1))
wealth <- flomarriage %v% 'wealth' # the %v% extracts vertex 
plot(flomarriage, vertex.cex = wealth/25+1, vertex.col = col20) #
flomodel.01 <- ergm(flomarriage ~ edges )
flomodel.01
invlogit <- function(x) {1/(1 + exp(-x))} 
x<-coef(flomodel.01)
invlogit(flomodel.01$coef[1])
set.seed(0) # 
flomodel.02 <- ergm(flomarriage ~ edges + triangle)
summary(flomodel.02)





#_______________________________________________________________________________________________________________________________________________________________________________________________________
### ---- Assignment task 1 ---- 
## Compute corresponding probabilities for the log-odds above. Are triangles significant in our model?

# if the tie will not add any triangles to the network, its log-odds is: -1.680
P1_1 <- invlogit(flomodel.02$coef[1])
P1_1
# if the tie will add one triangle to the network, its log-odds is: -1.680+0.169=-1.510
P1_2 <- invlogit(flomodel.02$coef[1]+flomodel.02$coef[2])
P1_2
# if a tie will add two triangles to the network, its log-odds is: -1.680+0.169x2=-1.341
P1_3 <- invlogit(flomodel.02$coef[1]+2*flomodel.02$coef[2])
P1_3
#Answer: the coefficient of triangles is not significant as p-value is 0.75 (>0.1)

#_______________________________________________________________________________________________________________________________________________________________________________________________________
### ---- Assignment task 2 ---- 
## Compute corresponding probabilities for the log-odds above. Are triangles significant in our model? 

class(flomodel.02) # check the class
names(flomodel.02) # look at the ERGM object
flomodel.02$coef
flomodel.02$formula


#_______________________________________________________________________________________________________________________________________________________________________________________________________
### ---- Assignment task 2 ---- 
## Explore the ERGM object on your own, testing at least 3-4 of the options you’ve generated with the names command. 
## What have you learned?
names(flomodel.02)
flomodel.02$etamap
flomodel.02$newnetworks
flomodel.02$loglikelihood
flomodel.02$gradient


#_______________________________________________________________________________________________________________________________________________________________________________________________________
### ---- Assignment task 3 ---- 
## Interpet the model results. Are the coefficients significant? 
## How does each component affect the probability of forming a tie? Calculate the corresponding probabilities.
flomodel.03 <- ergm(flomarriage~edges+nodecov('wealth'))
summary(flomodel.03)

# As both estimate coefficients are significant(p-value = 0.0001, 0.0241 < 0.1), we can claim: 
# - that with an addition of a tie, but without the addition of main effect of covariate the log odds is -2.594929, so the influence
# on the tie formation is negative. 
# - that without an addition of a tie, but with the addition of main effect of covariate the log odds is 0.010546, so the influence
# on the tie formation is positive

# We also can compute how those additions can change the probability of tie formation
exp(flomodel.03$coef[1])
# With the addition of a tie, but without the increase of sum of wealth of nodes i and j, we expect the probability 
# of tie formation to decrease by 93%.
exp(flomodel.03$coef[2])
# With the increase of sum of wealth of nodes i and j by one point, but zero ties, we expect the probability of tie formation
# to increase by 1.06%.


P3_edges <- invlogit(flomodel.03$coef[1])
P3_edges
# The corresponding probability of tie formation is 0,069 if sum of wealth attributes stays zero .
P3_attr <- invlogit(flomodel.03$coef[1]+flomodel.03$coef[2])
P3_attr
# The corresponding probability of tie formation is 0.07015 with one added tie and one added sum-of-wealths point.


#_______________________________________________________________________________________________________________________________________________________________________________________________________
### ---- Assignment task 4 ---- 
## Interpret the model results. Are the coefficients significant? 
## How does each component affect the probability of forming a tie? Calculate the corresponding probabilities.

data(samplk) 
plot(samplk3)

sampmodel.01 <- ergm(samplk3~edges+mutual)

summary(sampmodel.01)

# As both estimate coefficients are significant(p-value = 0.0001, 0.0001 < 0.1), we can claim: 
# - that with an addition of a tie, but without creating a mutual dyad, the log odds is -2.1692, so the influence
# on the tie formation is negative. 
# - that without an addition of a tie, and with creating a mutual dyad, the log odds is 2.3458, so the influence
# on the tie formation is positive

# We also can compute how those additions can change the probability of tie formation
exp(sampmodel.01$coef[1])
# With the addition of a tie, but zero wealth points, we expect the probability of tie formation to decrease by approximately
# 88%.
exp(sampmodel.01$coef[2])
# With the addition of a wealth point, but zero ties, we expect the probability of tie formation to increase by 920.506%.


P4_edges <- invlogit(sampmodel.01$coef[1])
P4_edges
# The corresponding probability of tie formation is 0.1030817 if it is not mutual.
P4_attr <- invlogit(sampmodel.01$coef[1] + sampmodel.01$coef[2])
P4_attr
# The corresponding probability of tie formation is 0,539 if it is mutual.








