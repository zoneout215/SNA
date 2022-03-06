#_______________________________________________________________________________________________________________________________________________________________________________________________________
### ---- HOME WORK ---- 

### ---- Assignment task 1 ---- 

## 1. Please carefully look at the Questionnaire. For the employees, 
## questions were broken down into groups, and groups were even named - 
## those are factors that we can analyze using Factor Analysis. Which of these
## factors could be used for good dependent variables? Why?

## Answer: We found several research papers, and from them we can derive that 
## emotional participation factor could be a good dependent variable. Work-flow
## quality, physical environment, salary, relationships with supervisors and 
## co-workers -- all of that can,in theory, affect emotional participation, or how it coined
## in the literature[see 1, 2] -- job satisfaction.


## 2. Please examine the supervisor’s questionnaire. In there, questions were not 
## grouped (by the way, can you tell the reason why?). Look at questions carefully,
## and using face validity, try to determine which questions would form separate and 
## theoretically justifiable factors. 

## Answer: The reason why those questions were not split into groups is that 
## we do not want to artificially construct those factors and put them into variables.
## Questions regarding supervisors' opinion measure a subjective entity, thus, presupposed
## factors would distort the research object. 

## Also from literature we can reconstruct theoretical factors from supervisors' questionnaire[3]:
## - Interpersonal helping Q13, Q21 - Q23
## - Individual initiative Q16-Q19, Q24-Q32
## - Personal industry Q4-Q12, Q14, Q33
## - Loyal boosterism Q1- Q4


## Literature:
## 1) Smith C., Organ D. W., Near J. P. Organizational citizenship behavior: Its nature and antecedents 
## //Journal of applied psychology. – 1983. – Т. 68. – №. 4. – С. 653.
## 2) Macdonald S., Maclntyre P. The generic job satisfaction scale: Scale development and its correlates 
## //Employee Assistance Quarterly. – 1997. – Т. 13. – №. 2. – С. 1-16.
## 3) Moorman R. H., Blakely G. L. Individualism‐collectivism as an individual difference predictor of organizational 
## citizenship behavior //Journal of organizational behavior. – 1995. – Т. 16. – №. 2. – С. 127-142.


suppressPackageStartupMessages(library(ergm))
suppressPackageStartupMessages(library(coda))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(rgl))
suppressPackageStartupMessages(library(sna))
suppressPackageStartupMessages(library(knitr))



# HOMEWORK 2
library('igraph')
library('Hmisc')

### ---- Assignment task 2 ---- 

all_net<-read.csv('AllNet.csv', header=TRUE, sep=";") # read data
rownames(all_net) <- all_net[,1]
all_net[,1] <- NULL
all_net<-as.matrix(all_net) # save it as a matrix
all_graph<-graph_from_adjacency_matrix(all_net) #create a graph
head(all_graph)


FriendMat<-read.csv("Friendnet.csv",header=TRUE, sep=";")
rownames(FriendMat) <- FriendMat[,1]
FriendMat[,1] <- NULL

View(FriendMat)
ncol(FriendMat)
nrow(FriendMat)
summarize(FriendMat)

ProfMat<-read.csv("Profnet.csv",header=TRUE, sep=";")
rownames(ProfMat) <- ProfMat[,1]
ProfMat[,1] <- NULL

BossMat<-read.csv("BossNet.csv",header=TRUE, sep=";")
rownames(BossMat) <- BossMat[,1]
BossMat[,1] <- NULL


SupportMat<-read.csv("SupportNet.csv",header=TRUE, sep=";")
rownames(SupportMat) <- SupportMat[,1]
SupportMat[,1] <- NULL



FriendMat<-as.matrix(FriendMat)

ProfMat<-as.matrix(ProfMat)
BossMat<-as.matrix(BossMat)
SupportMat<-as.matrix(SupportMat)
Friend.any <- ifelse(FriendMat > 0, 1, 0)
Boss.any <- ifelse(BossMat > 0, 1, 0)
Prof.any <- ifelse(ProfMat > 0, 1, 0)
Support.any <- ifelse(SupportMat > 0, 1, 0)
suppressPackageStartupMessages(library(igraph))

FriendGraph<-graph_from_adjacency_matrix(FriendMat,  weighted=TRUE)
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

ocb_att<-read.csv('OCB_att.csv', header=TRUE)
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


ocb_att$Education
ocb_att$Title
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



detach(package:sna)
detach(package:ergm)
detach(package:network)

detach(package:igraph)

plot(Profnet, vertex.size=.1)

set.seed(0)
proff_model <-ergm(Profnet ~ edges)
summary(proff_model)

invlogit <- function(x) {1/(1 + exp(-x))} 
x<-coef(friend_model)

invlogit(friend_model$coef[1])

proff_model <- ergm(Profnet ~ edges + mutual)
summary(proff_model)

mcmc.diagnostics(proff_model)

proff_model.01  <- ergm(Profnet~edges+ mutual+gwesp(0.25) + nodecov('age'),
     control = control.ergm(seed=1,MCMC.samplesize=50000, MCMC.interval=1000), verbose =T) 
summary(proff_model.01)


all_net %v% "sex" <- sex
all_net %v% "sex" <- age

Friendnet %v% "sex" <- sex 
Friendnet %v% "age" <- age
Friendnet %v% "secondary specialized" <- ed1 
Friendnet %v% "higher ed" <- ed2 
Friendnet %v% "t_position" <- tenure
Friendnet %v% "t_organization" <- tenure_org
Friendnet %v% "t_supervisor" <- tenure_sup
Friendnet %v% "emo_participation" <- Emotional_part
Friendnet %v% "title" <- ocb_att$Title 

Profnet %v% "sex" <- sex 
Profnet %v% "age" <- age 
Profnet %v% "title" <- ocb_att$Title 
Profnet %v% "ed" <- ocb_att$Education 
Profnet %v% "secondary specialized" <- ed1 
Profnet %v% "higher ed" <- ed2 
Profnet %v% "t_position" <- tenure
Profnet %v% "t_organization" <- tenure_org
Profnet %v% "t_supervisor" <- tenure_sup
Profnet %v% "emo_participation" <- Emotional_part
Profnet %v% "Work_Quant" <- Work_Quant
Profnet %v% "Work_Resp" <- Work_Resp
Profnet %v% "Work_Diff" <- Work_Diff
Profnet %v% "Work_Speed" <- Work_Speed
Profnet %v% "Admin_problems" <- Admin_problems
Profnet %v% "Personal_conflicts" <- Personal_conflicts




Bossnet %v% "sex" <- sex 
Bossnet %v% "age" <- age
Bossnet %v% "secondary specialized" <- ed1 
Bossnet %v% "higher ed" <- ed2 
Bossnet %v% "t_position" <- tenure
Bossnet %v% "t_organization" <- tenure_org
Bossnet %v% "t_supervisor" <- tenure_sup

Supportnet %v% "sex" <- sex 
Supportnet %v% "age" <- age
Supportnet %v% "secondary specialized" <- ed1 
Supportnet %v% "higher ed" <- ed2 
Supportnet %v% "t_position" <- tenure
Supportnet %v% "t_organization" <- tenure_org
Supportnet %v% "t_supervisor" <- tenure_sup





set.seed(0)
prof_model.03 <- ergm(Profnet ~ edges + mutual) 
summary(prof_model.03)

prof_model.04 <- ergm(Profnet ~ edges + mutual + nodefactor('sex') + nodecov('age')+ nodematch('secondary specialized')
                      +  nodematch('higher ed')+ nodecov('t_position')+nodecov('t_organization')+nodecov('t_supervisor') )
summary(prof_model.04)

prof_model.05 <- ergm(Profnet ~ edges +mutual + nodeicov('age')+ nodeocov('age') + nodefactor('title'))
summary(prof_model.05)
prof_model.05.gof<-gof(prof_model.05~odegree)

prof_model.06 <- ergm(Profnet ~ edges + mutual  + nodecov('age')+ nodefactor('sex')  )
summary(prof_model.06)

prof_model.07 <- ergm(Profnet ~ edges + mutual+ nodecov('age')+ nodefactor('ed')  )
summary(prof_model.07)

prof_model.08 <- ergm(Profnet ~ edges + mutual  + nodecov('age')+nodematch('sex')+ gwesp(0.5, fixed=T),
                      control = control.ergm(seed=1,MCMC.samplesize=50000,MCMC.interval=1000), verbose = T))
summary(prof_model.08)

count(sex)
prof_model.08 <- ergm(Profnet ~ edges + mutual + nodecov('age') + nodematch('sex') )
summary(prof_model.08)







friend_model.01<- ergm(Friendnet ~ edges + mutual  + nodecov('age')+ nodecov('emo_participation') + nodematch('sex') )


prof_model.08.gof<-gof(prof_model.08~triadcensus)
# The code below is commented out, but if you want to see
# what we get with m1.gof object, check it:
##names(m1.gof)
kable(prof_model.08.gof$pval.ideg, caption="Goodness-of-fit for Indegree")
par(mfrow=c(1,1))
plot(prof_model.08.gof)


prof_model.09 <- ergm(Profnet~edges+mutual+nodecov('age') +gwesp(0.5,fixed=T), control=control.ergm(seed=0,MCMLE.maxit=1, MCMLE.density.guard.min=20000) )

summary(prof_model.08)
mcmc.diagnostics(prof_model.08)


summary(friend_model.01)
Profnet %v% "Work_Speed" <- Work_Speed
Profnet %v% "Admin_problems" <- Admin_problems
Profnet %v% "Personal_conflicts" <- Personal_conflicts


mcmc.diagnostics(prof_model.07)




f1.gof<-gof(friend_model.02~odegree)
kable(f1.gof$pval.ideg, caption="Goodness-of-fit for Indegree") 
par(mfrow=c(1,1))
plot(f1.gof)

