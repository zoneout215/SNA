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




### ---- Assignment task 1 ---- 

##
##You should be able to easily answer the following questions:

##  1. What is an odds function?
## Answer: Odds function is equivalent to the exponential function of OLS 
## regression. So Odds = exp(b0 + b1*x1 + ... + bn*xn), where odds = p(x)/1-p(x), 
## given that p(x) is a probability that dependent variable equals a case.

##  2. What is an odds ratio?
## Answer: In case of a continuous independent variable the odds ratio is defined 
## as OR = odds(x+1)/odds(x), where odds(x) = p(x)/1-p(x) = exp(b0 + b1*x1). 
## The bn*xn is skipped for simplicity, so OR = exp(b0 + b1*(x1+1))/exp(b0 + b1*x1), 
## which gives us the final form OR = e(b1). In case of binary independent variable,
## OR = a*b/c*d, where a, b, c, d are the probabilities in the contingency table of independent
## variable. 

##  3. What is a log-odds function?
## Logs-odds function is  defined as log(p(x)/1-p(x)) = b0 + b1*x1 + ... + bn*xn
##  4. What is a logistic function?
## Logistic function has a sigmoid shape and is defined as -- again the case of 
## one dependent variable is taken for simplicity, -- P(x)= 1/1+exp(-(b0 + b1*x1))
##  5. What is the relationship between predicted probabilities and predicted odds? 

## 6. What is a pseudo-R-square?

##  7. How do you interpret the results of the logistic regression?
  


#_______________________________________________________________________________________________________________________________________________________________________________________________________
### ---- Assignment task 2 ---- 
## Compute corresponding probabilities for the log-odds above. Are triangles significant in our model?

# if the tie will not add any triangles to the network, its log-odds is: -1.680
P2_1 <- invlogit(flomodel.02$coef[1])
P2_1
# if the tie will add one triangle to the network, its log-odds is: -1.680+0.169=-1.510
P2_2 <- invlogit(flomodel.02$coef[1]+flomodel.02$coef[2])
P2_2
# if a tie will add two triangles to the network, its log-odds is: -1.680+0.169x2=-1.341
P2_3 <- invlogit(flomodel.02$coef[1]+2*flomodel.02$coef[2])
P2_3
#Answer: the coefficient of triangles is not significant as p-value is 0.75 (>0.1)

#_______________________________________________________________________________________________________________________________________________________________________________________________________
### ---- Assignment task 3 ---- 
## Explore the ERGM object on your own, testing at least 3-4 of the options you’ve generated with the names command. 
## What have you learned?

class(flomodel.02) # check the class
names(flomodel.02)
flomodel.02$etamap
flomodel.02$newnetworks
flomodel.02$loglikelihood
flomodel.02$gradient

#_______________________________________________________________________________________________________________________________________________________________________________________________________
### ---- Assignment task 4 ---- 
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


P4_edges <- invlogit(flomodel.03$coef[1])
P4_edges
# The corresponding probability of tie formation is 0,069 if sum of wealth attributes stays zero .
P4_attr <- invlogit(flomodel.03$coef[1]+flomodel.03$coef[2])
P4_attr
# The corresponding probability of tie formation is 0.07015 with one added tie and one added sum-of-wealths point.


#_______________________________________________________________________________________________________________________________________________________________________________________________________
### ---- Assignment task 5 ---- 
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


P5_edges <- invlogit(sampmodel.01$coef[1])
P5_edges
# The corresponding probability of tie formation is 0.1030817 if it is not mutual.
P5_attr <- invlogit(sampmodel.01$coef[1] + sampmodel.01$coef[2])
P5_attr
# The corresponding probability of tie formation is 0,539 if it is mutual.

# HOMEWORK 2
library('igraph')

all_net<-read.csv('AllNet.csv', header=TRUE, sep=";") # read data

all_mat<-as.matrix(all_net) # save it as a matrix

transform(all_mat, YuEYu = as.numeric(YuEYu))

all_mat[is.na(all_mat)] <- 0

all_graph<-graph_from_adjacency_matrix(all_mat) #create a graph

FriendMat<-read.csv("Friendnet.csv",header=TRUE, sep=";")
ProfMat<-read.csv("Profnet.csv",header=TRUE, sep=";")
BossMat<-read.csv("BossNet.csv",header=TRUE, sep=";")
SupportMat<-read.csv("SupportNet.csv",header=TRUE, sep=";")
FriendMat<-as.matrix(FriendMat)
ProfMat<-as.matrix(ProfMat)
BossMat<-as.matrix(BossMat)
SupportMat<-as.matrix(SupportMat)
Friend.any <- ifelse(FriendMat > 0, 1, 0)
Boss.any <- ifelse(BossMat > 0, 1, 0)
Prof.any <- ifelse(ProfMat > 0, 1, 0)
Support.any <- ifelse(SupportMat > 0, 1, 0)
suppressPackageStartupMessages(library(igraph))
FriendGraph<-graph_from_adjacency_matrix(FriendMat, weighted=TRUE)
ProfGraph<-graph_from_adjacency_matrix(ProfMat, weighted=TRUE)
BossGraph<-graph_from_adjacency_matrix(BossMat, weighted=TRUE)
SupportGraph<-graph_from_adjacency_matrix(SupportMat, weighted=TRUE)
FriendGraph.any <-graph.adjacency(Friend.any,
                                  mode=c("directed"),
                                  weighted=NULL,
                                  diag=FALSE)


BossGraph.any <-graph.adjacency(Boss.any,
                                mode=c("directed"),
                                weighted=NULL,
                                diag=FALSE)
ProfGraph.any <-graph.adjacency(Prof.any,
                                mode=c("directed"),
                                weighted=NULL,
                                diag=FALSE)
SupportGraph.any <-graph.adjacency(Support.any,
                                   mode=c("directed"),
                                   weighted=NULL,
                                   diag=FALSE)


detach(package:igraph)
library(sna)
Friendnet<-as.network(Friend.any, directed=TRUE)
Bossnet<-as.network(Boss.any, directed=TRUE)
Profnet<-as.network(Prof.any, directed=TRUE)
Supportnet<-as.network(Support.any, directed=TRUE)

ocb_att<-read.csv('ocb_att.csv', header=TRUE)
# To make sure we got it right, let's look at the age variable:
ocb_att$Age


age<-ocb_att$Age
sex<-ocb_att$Sex
#How long the person had this position:
tenure<-ocb_att$WorkTitleYear+ocb_att$WorkTitleMonth/12
#How long worked in organization:
tenure_org<-ocb_att$WorkOrgYear+ocb_att$WorkOrgMonth/12
#How long reported to the same supervisor:
tenure_sup<-ocb_att$RepSupYear+ocb_att$RepSupMonth/12
# Set of dummies for education:
ed1<-ifelse(ocb_att$Education==3,1,0) # this is for secondary specialized
ed2<-ifelse(ocb_att$Education==4,1,0) # this is higher
# Secondary, obviously, is the baseline

#Physical participation variable
phys_part<-ocb_att$Phys_Part


HR_att<-read.csv("OCB_att.csv",header=TRUE)
#Let's get the sex of our respondents into its own vector:
sex<-HR_att$Sex
age<-HR_att$Age
#Dependent variables
Emotional_part<-HR_att$Emot_Part
Intent_to_leave<-HR_att$Intent_toLeave
Personal_conflicts<-HR_att$Personal_conflicts
#Predictors
#Challenge stressors:
Work_Quant<-HR_att$Work_quant #Work quantity
Work_Resp<-HR_att$Work_Resp #Work responsibility
Work_Diff<-HR_att$Work_Diff #work difficulty
Work_Speed<-HR_att$Work_Speed # Work speed
#Hindrance stressors:
Admin_problems<-HR_att$Admin_problems
Personal_conflicts<-HR_att$Personal_conflicts


names<-ocb_att$Name # pull the names out of attributes dataset
gender_vector<-vector() #create a vector for gender


suppressPackageStartupMessages(library(igraph))
for(i in 1:122){ # this is our set of all network nodes
  for(j in 1:68){ # this is our set of attribute-containing nodes
    # for each node in i, we run through all node in j
    # and compare names
    if(V(all_graph)$name[i]==names[j]){
      #if we match, we add the attribute to a vector
      gender_vector[i]<-sex[j]
      # and exit the inner loop for the next i
      break;}
    # if not, we are assigning a missing value
    # and keep going until we find a matching node
    else{gender_vector[i]<-NA}
  }
}
# Let's look at the result:
gender_vector






