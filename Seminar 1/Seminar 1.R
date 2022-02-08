install.packages('sna')
install.packages('network')
install.packages('foreign')
install.packages('igraph')
install.packages('ndtv')
install.packages('RColorBrewer')
install.packages('rgl')
install.packages('ergm')

library('sna')
library('network')
library('foreign')
library('igraph')
library('ndtv')
library('RColorBrewer')
library('rgl')
install.packages('ergm')

# Хар-ки рандомной сети - примерно одинаковое кол-во связей у каждого узла,
# никакие структурные компоненты рандомной сети не будут видны, это будет
# примерно равномерно распространенное нечто

# Структура данных для матрицы - матрица. Считаем всегда слева направо
# формирующие связи узла - это строчка, поэтому матрица может быть несимметричная
# по матрице смотрим по строчкам из какого узла к какому идут связи, входящие
# смотрим по строчкам

# Самое главное - чтобы колонки и строчки шли в одинаковом порядке, потому что
# машина не считывает заголовки столбцов и строчек

# Близость двух узлов образуется не по каким-то расчетам а-ля евклидового
# расстояния, а просто по наличию связи

# Если матрица симметричная - сеть будет с ненаправленными связями

num_nodes <- 10
my_matrix<-matrix(round(runif(num_nodes*num_nodes)), # edge values
                  nrow = num_nodes, #nrow must be same as ncol
                  ncol = num_nodes)
dim(my_matrix)
diag(my_matrix)

my_network<-as.network(x = my_matrix, # the network object
                       directed = TRUE, # specify whether the network is directed
                       loops = TRUE, # do we allow self ties (should not allow them)
                       matrix.type = "adjacency" # the type of input
)

par(mar=c(1,1,1,1)) # get rid of the large margins and plot the data:
plot(my_network)
network.density(my_network)
class(my_network)

install.packages('intergraph')
library('intergraph') # с этим пакетом можно перевести объект из одного типа
# в другой

# Рандомные графы нужны для того, чтобы сравнить сети и понять, что наша сеть
# не рандомна

drugpaj <- read.paj('drugnet2.paj') # команда сразу формирует сеть или 
# атрибуты узлов
drug <- drugpaj$networks[[1]] # extract network
library('network')
plot(drug)
##_________________________________________________________________________________________________________________________________________________________________________________
# ----- Assignment question 1 ----- 
# what looks strange about this network? Why?
# Answer:  Almost a half of nodes are not connected. The fact that those individuals are not 
#acquainted looks strange, because, even from the economics perspective, consumers need some 
#connections with the supplier. In case of such hard to reach goods, the lack of ties looks 
#suspicious.

network.size(drug) # how many nodes?
network.edgecount(drug)
network.density(drug)
network.dyadcount(drug) # how many dyads?
##_________________________________________________________________________________________________________________________________________________________________________________
#----- Assignment question 2-----
# What do the numbers above represent?

network.size(drug) # how many nodes?
network.edgecount(drug)# how many edges?
network.dyadcount(drug) # how many dyads?

# Answer:The  number of observed individuals, the number of actual pairs of acquaintances, and 
# the number of possible pairs of acquaintances.

gender<-drugpaj$partitions[[1]] #extract the attributes
ethnicity <- drugpaj$partitions[[2]]

table(gender)
table(ethnicity)

colors1 <- ifelse(ethnicity==1, "red", ifelse(ethnicity==2, "green", 
                                              ifelse(ethnicity==3, "yellow",
                                                     "blue")))
plot(drug, vertex.col=colors1)

colors2 <- ifelse(gender==1, "blue", ifelse(gender==2, "red", "gray"))
plot(drug, vertex.col=colors2)

# Посмотреть плотность по отдельным элементам (например, по расе) - команда 
# components




load('Flo.rdata')
flomarriage <- as.network(as.matrix(flo.marriage), directed=FALSE)
plot(flomarriage, displaylabels=TRUE)
set.vertex.attribute(flomarriage, 'wealth', flo.att[,2])
flomarriage
plot(flomarriage)


install.packages('RColorBrewer')
library('RColorBrewer')
FloColors <- c(brewer.pal(11,'RdYlBu'),brewer.pal(9,'RdYlBu')) #set a vector of colors
par(mar=c(0,0,0,0))
plot(flomarriage,
    vertex.cex=(get.vertex.attribute(flomarriage, 'wealth')/25 +.4), 
    displaylabels=TRUE,
    label.cex=.5,
    label.pos=0,
    vertex.col=FloColors)
display.brewer.all()
##_________________________________________________________________________________________________________________________________________________________________________________
# ----- Assignment task 3 ----- 
# You may have noticed that there is another network in that data file - the business network. 
#Please plot the biz network with node attributes that you’ve set above.

flobis <- as.network(as.matrix(flo.biz), directed=FALSE)
plot(flobis, displaylabels=TRUE)
set.vertex.attribute(flobis, 'wealth', flo.att[,2])
flobis
plot(flobis)

plot(flobis,
     vertex.cex=(get.vertex.attribute(flobis, 'wealth')/25 +.4), 
     displaylabels=TRUE,
     label.cex=.5,
     label.pos=0,
     vertex.col=FloColors)

##__________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
# ----- Assignment task 4 ----- 
# Using the code already shown, plot both of the new networks. Add attributes if you wish.

flo.names <- scan('padgett.dat', what='character', skip=4, nlines=16) # Read data with read.table()
flos <- read.table('padgett.dat', skip=41, col.names=flo.names)
# Read node attributes:
flo.att <- read.table('padgw.dat',
                      col.names =c('WEALTH','NUM.PRIORS','NUM.TIES'), skip=25) 
flo.att

flo.att <-cbind(flo.names,flo.att)

head(flo.att)

# Separate adjacency matrices
# subset of the first 16 colums is the marriage network flo.marriage <-flos[1:16,]
dim(flo.marriage)


row.names(flo.marriage) <-flo.names # name
flo.biz <- flos[17:32,] # subset of the second 16 is the business network. row.names(flo.biz) <-flo.names # name
dim(flo.biz)

# Check the data by listing a couple of rows and columns from each network.
flo.marriage[1:2,1:2]
flo.marriage[15:16,15:16]
flo.biz[1:2,1:2]
flo.biz[15:16,15:16]


flo.marriage <- as.network(as.matrix(flo.marriage),directed=FALSE) 
flo.biz <- as.network(as.matrix(flo.biz),directed=FALSE)
## add attributes
set.vertex.attribute(flo.marriage, 'wealth', flo.att[,2]) 
set.vertex.attribute(flo.biz,'wealth', flo.att[,2])


par(mar=c(0,0,0,0))
plot(flo.marriage,
     vertex.cex=(get.vertex.attribute(flo.marriage, 'wealth')/25 +.4), displaylabels=TRUE,
     label.cex=.5,
     label.pos=0, vertex.col=FloColors)

par(mar=c(0,0,0,0)) 
plot(flo.biz,
     vertex.cex=(get.vertex.attribute(flo.biz, 'wealth')/25 +.4), displaylabels=TRUE,
     label.cex=.5,
     label.pos=0, vertex.col=FloColors)



##__________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
# ----- Assignment task 5 ----- 
# For the network “drug” that we created and loaded with attributes, create several different network plots, adding gender and ethnicity 
# to the graph as node attributes. Consider using a variety of colors to make your plot more informative.
drugpaj <- read.paj('drugnet2.paj') #read the data
drug <- drugpaj$networks[[1]] # extract network
gender<-drugpaj$partitions[[1]] #extract the attributes 
install.packages('knitr')
suppressPackageStartupMessages(library(knitr)) #allows for better-looking tables 
kable(table(gender), col.names=c("Gender","Frequency"))

gender<-drugpaj$partitions[[1]] #extract the attributes
ethnicity <- drugpaj$partitions[[2]]

table(gender)
table(ethnicity)

#Set vectors based on attributes.
#Number of node sides allows to create different shapes
#(3=triangle, 4=square, etc.)

# Plot of ’Hartford Drug Users’ network with attributes
sides<-ifelse(ethnicity==1,12, ifelse(ethnicity==2, 3, ifelse(ethnicity==3, 4, 6)))
#Set colors by gender, including gray for unknown:
colors<-ifelse(gender==2,"palevioletred",ifelse(gender==1,"royalblue2","gray8"))
par(mar=c(0,0,0,0)) # And the plot itself:
plot(drug, vertex.col=colors, vertex.sides=sides, vertex.cex=1.5)


# ---Better visualization of multi-category attributes, ’Hartford Drug Users’ network---
colors2<-ifelse(ethnicity==1,"red", ifelse(ethnicity==2, "green", ifelse(ethnicity==3, "blue", "yellow")))
sides2<-ifelse(gender==2,12,ifelse(gender==1,3,4))
par(mar=c(0,0,0,0)) # And the plot itself:
plot(drug, vertex.col=colors2, vertex.sides=sides2, vertex.cex=1) 

### ----- DRAGON-----
# у меня это так и не сработало, пакет рушит ар
library('ggplot2')
##A simple 3D plot:
gplot3d(flomarriage, vertex.col=FloColors,
        vertex.radius=1.5, edge.lwd=.2)
##Add names
gplot3d(flomarriage, vertex.col=FloColors,
        vertex.radius=1.5, edge.lwd=.2, displaylabels=TRUE)
##Add wealth
gplot3d(flomarriage, vertex.col=FloColors,
vertex.radius=(get.vertex.attribute(flomarriage, 'wealth')/30 +.5),
edge.lwd=.2, displaylabels=TRUE)


# киношка
library('ndtv')
drakon=list()
for (i in 1:65) {
    drakon[[i]]<-read.paj(paste("T",i,".paj",sep=""))
    }
drakNet<-networkDynamic(network.list=drakon, start=1)

activity<-read.csv("Activity.csv")
activity<-activity[,4:68]
for (i in 1:65) {
    activate.vertex.attribute(drakNet,'activity',activity[[i]],onset=i,terminus=i)}


render.d3movie(drakNet, plot.par=list(displaylabels=T,
vertex.col=FloColors, vertex.cex = function(slice){slice%v%'activity'/5+0.5}))

# Семинар 2

load('trade.Rdata')

##_________________________________________________________________________________________________________________________________________________________________________________
# ----- Assignment question 1 ----- 
# Why did we use option FALSE for command “directed” above, when creating a network?
# Answer: Because of the nature of  ties; sexual relationship does not have any direction 
# as it require two persons, thus we do not need any direction in our graph.

##_________________________________________________________________________________________________________________________________________________________________________________


##_________________________________________________________________________________________________________________________________________________________________________________
# ----- Assignment task 2----- 
## Please examine the options in the “network.layout”” command and perform the following:
## 1. Create the madmen.net with labels.
## 2. Experiment with options by adding attributes, changing vertex or edge colors,
## finding the best position for labels. While this task may take a while, it will 
## count as complete if you generate at least one graph that is different from the 
## graphs I’ve shown you in this assignment. The more different graphs with options 
## you generate, the better - extra practice never hurts anyone.

load('madmen.Rdata')

colPastel<-brewer.pal(11, 'Spectral')
mad.net <- as.network(mad.matrix, directed=FALSE)
set.vertex.attribute(mad.net,attrname='female', value=mad.att[,2])


colors <- ifelse(mad.att$Female == 0, colPastel[10], colPastel[1])
sides<-ifelse(mad.att$Female == 0, 12, 3)

par(mar=c(0,0,0,0))
plot(mad.net, vertex.col = colors)

plot(mad.net,
     displaylabels=TRUE,
     label.cex=.6,
     label.pos=0,  
     vertex.col=colors,
     vertex.sides=sides,
     mode='fruchtermanreingold')

##_________________________________________________________________________________________________________________________________________________________________________________
# ----- Assignment task 3----- 
## Please examine available matrices and answer the following questions:
## 1. Are the matrices symmetric?
## 2. What does that mean for resulting networks? Would they be directed or undirected?

load('trade.Rdata')

isSymmetric(as.matrix(trade.all))
isSymmetric(as.matrix(manufacture))
isSymmetric(as.matrix(food))
isSymmetric(as.matrix(crude ))
isSymmetric(as.matrix(minerals ))
isSymmetric(as.matrix(diplomacy ))

## Answer: It means that a given individual have a connection with another, 
## but it is possible that they are not mutually connected, thus network should be undirected.

##_________________________________________________________________________________________________________________________________________________________________________________
# ----- Assignment task 4----- 
## With respect to the above actions, please answer the following:
## 1. How would you justify any of these choices? Please refer to specific social theories to make your answer more legitimate.
## 2. What are the empirical implication of these choices?

load('trade.Rdata')
trade.all<-as.matrix(trade.all)

trade.any <- ifelse(trade.all > 0, 1, 0)
### Answer: Here we convert our data to the criteria if a country have any connection with another one. In order to justify such choice, we can refer to the 
### actor-network theory implemented by Bruno Latour. According to ANT the actor's conectedness to other actors is more important than its immanent characteristics, 
### so if we need to check if the actors, in our case -- countries -- have at least some connections, we can implement such dichotomization.   

trade.2 <- ifelse(trade.all > 1, 1, 0)
trade.max <-ifelse(trade.all == 5, 1, 0)

### Answer: What is more, if the object of our interest is either strenth or weakness of connectedness, keeping in mind the theory of M. Granovetter, we can chose 
### the cutpoint, which will refer to the difference between strong and weak ties. 
### With regard to the emprical aspects, the first dichotomy represents the existence/absence of tie between two countries, the second dichotomy will be relevant 
### if we pose such research question, that the connection will be imortant for us only if countries have at least two parameters of trades, other cases will 
###be ignored. Finally, in the last case, as in the second, we need a research question, that will enable us to ignore a particular amount of parameters. Basically,
### we can conceptialize this dichotomy as "trade alies"/ "not trade alies" if we operationalise trade aliens as a cutpoint at 5 parameters.


### Literature:
### 1)Latour B. Reassembling the social: An introduction to actor-network-theory. – Oup Oxford, 2007.
### 2)Granovetter M. S. The strength of weak ties //American journal of sociology. – 1973. – Т. 78. – №. 6. – С. 1360-1380.


# ---- Assignment task 5 ----- 
## Irrespective of all the color/shape variations that are hurting your eyes 
## (but at the same time show you the capabilities of the package), please 
## answer the following questions:
##  1. What differences do you observe between the graphs where the cutpoint
## is any tie, at least two ties, and all ties present?

## The first and the most obvious difference is that graph is the way more 
## readable in comparison with the others, due to a smaller number of both
## nodes and linkes.
## On the last graph "outsiders" become noticable - contries, who receive more 
## than give. But in the same time three centers - Spain, USA and UK become
## noticable.
## On other graphs it is much harder to find any of the relationship patterns, 
## but "outsiders" are still noticable, nevertheless not we can see that they 
## do not only receive, but have one or two (rarely more than two) countries,
## with who they have outgoing linkage or sometimes two-sided.

##  2. What information can you gather from these observed differences to help 
## you expand on your earlier theoretical justification of ties? Alternatively,
## does your theoretical justification seem reasonable in light of new
## information obtained from these graphs?
  
## The most important conclusion is that there is no one right way of 
## dichotomization, because from different points of view we can accidentally 
## draw the wrong conclusions, for example, if we were were studying Madagascar,
## looking on the last graph we can draw a conclusion that it does not has
## any outcoming relationships with anyone, but on the second graph it is
## obvious that Madagascar has outcoming relationships at least with USA


##_________________________________________________________________________________________________________________________________________________________________________________
# ----- Assignment task 6----- 
## Of course, there are differences between directed and undirected networks on the graph and with stats. Please
## answer the following questions:
## 1. Draw directed and undirected ‘tradenet.2‘ networks side by side.
## 2. What are the differences in graphs and how would you interpret them?
library('sna')
library('network')
library('foreign')

load('trade.Rdata')

trade.2 <- ifelse(trade.all > 1, 1, 0)

tradenet.sym.2<- network(trade.2, directed=FALSE)
tradenet.sym.3<- network(trade.2, directed=TRUE)

par(mfrow=c(1,2))
par(mar=c(0,0,0,0))

plot(tradenet.sym.2,
     displaylabels=TRUE,
     label.cex =.5,
     edge.col = 'gray50')

plot(tradenet.sym.3,
      displaylabels=TRUE,
      label.cex =.5,
      edge.col = 'gray50')

# The main difference in graphs is that now with directions being visible we can understand that
# diplomatical connections do not have to be symmetrical, which means - having connection does not include
# mirroring them from both sides/countries/actors at the ends of ties 

##_________________________________________________________________________________________________________________________________________________________________________________
# ----- Assignment task 7----- 
## What are the differences between the three networks - why do we have more components in the last network?
## How would you explain them from the theoretical level?
  

trade.all<-as.matrix(trade.all)
trade.any <- ifelse(trade.all > 0, 1, 0)
trade.max <- ifelse(trade.all == 5, 1, 0)


tradenet.any<-network(trade.any)
tradenet2<-network(trade.2)
tradenet5 <- network(trade.max) #turn it to a network also

components(tradenet.any)
components(tradenet2)
components(tradenet5)


# Looks like components there are based on number of ties (line-generated subgraphs), 
# so dichotomization in trade.max leads to disconnection of components/formation of new ones as subgraphs change

##_________________________________________________________________________________________________________________________________________________________________________________
# ----- HOME WORK 1 ----- 
### This is your first homework, and you will note that the amount of work is a bit higher than for a seminar. Perhaps, not in volume, but in integration - we ask you to put together everything that you know. You can still work in groups (same rules apply as for the seminar), and you can submit both the seminar assignment and the homework in the same file.
### There are several networks in the “trade.Rdata” file, described above. We have fully explored the “trade.all” network. Now, select one of the individual trade networks (manufacture, food, crude, etc.) and show me everything you’ve learned in this class so far. At the very minimum, please do the following:

### 1. Using package ‘sna‘, plot the network. Use attributes to color it or change the shapes of the nodes (attribute data are in the attributes file).
# 1.
load('trade.Rdata')

diplomacy<-as.matrix(diplomacy)
diplomacynet<- network(diplomacy, directed=TRUE)

set.vertex.attribute(diplomacynet, 'GNP', trade.att[,2])

col1 <- brewer.pal(11,'Spectral')
par(mar=c(0,0,0,0))
plot(diplomacynet,
     displaylabels=TRUE,
     label.cex =.5,
     vertex.col = col1,
     vertex.cex=(get.vertex.attribute(diplomacynet, 'GNP')/2 + .4),
     edge.col = "#7C8083")





### 2. Using package ‘igraph‘, create an appropriate graph with all possible options.

detach(package:sna)
detach(package:network)
library(igraph)

tradegraph.diplomacy <-graph.adjacency(diplomacy,
                                 mode=c("directed"),
                                 weighted=NULL,
                                 diag=FALSE)

## here we generate all possible layouts, in order to check the most suitable one.We do not immediatly use layout_nicely 
## command, as it should be checked if it gives the most appropriate graph.
l1<- layout_in_circle(tradegraph.diplomacy)
l2<- layout_with_fr(tradegraph.diplomacy)
l3<- layout_as_tree(tradegraph.diplomacy)
l4<- layout_with_lgl(tradegraph.diplomacy)
l5<- layout_with_kk(tradegraph.diplomacy)
l6<- layout_nicely(tradegraph.diplomacy)

x <- list(l1,l2,l3,l4,l5,l6)

for (k in x) {   
  plot(tradegraph.diplomacy,
       vertex.size = 6,
       edge.arrow.size = .3,
       edge.color='gray',
       vertex.label.cex = .5,
       vertex.color = 'Blue',
       vertex.shape = 'circle',
       vertex.frame.color = 'white',
       vertex.label.dist = .5,
       vertex.label.color = 'black', 
       layout = k)
}

## Here we create an attribute of the amount of directed ties -- both ins and outs.
g.alld <- degree(tradegraph.diplomacy, mode = c("all"))

# Make a histogram of degree
hist(g.alld, breaks = 30)

# Find the vertex that has the maximum degree
which.max(g.alld)
## That creates a palette for three categories of countries with regard to the amount of schools, [0, 30], (30, 70] and (70, n]
colb<-brewer.pal(9, "Blues")
colorsblues_schools <- ifelse(trade.att[,3] <= 30, colb[2], ifelse(trade.att[,3] > 30 &trade.att[,3]  <= 70 , colb[5],
                                                              ifelse(trade.att[,3] > 70 ,  colb[7],  "gray8")))
plot(tradegraph.diplomacy,
     vertex.size = 6,
     edge.arrow.size = .3,
     edge.color='gray',
     vertex.label.cex = .5,
     vertex.color = colorsblues_schools,
     vertex.shape = 'circle',
     vertex.frame.color = 'white',
     vertex.label.dist = .5,
     vertex.label.color = 'black', 
     layout = l6)

### 3. Tell us what inferences you can make about your selected network based on the information you’ve obtained. Supplement your arguments with logic and theory. 
### To get full credit, this part should be no shorter than 3 pages, double-spaced, with graphs and other displays necessary.
## Answer: According to the theory of Acemoglu D., the more inclusive political institutions country has, the more prosperity chances 
## it has throughout the development of history. As a consistent democratically inclined, Acemoglu treats open acccess to education, inclusiveness 
## and constructive political dialogue as immanent traces of progressive economic growth. Bearing that in mind, it seems logical that contries with more schools,
## tend to be more inclined to the political dialogue with other counties.
 
### 	Acemoglu D., Robinson J. A., Woren D. Why nations fail: The origins of power, prosperity and poverty. – New York : Crown Publishers, 2012. – Т. 4.
