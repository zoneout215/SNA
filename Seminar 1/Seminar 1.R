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


##_________________________________________________________________________________________________________________________________________________________________________________
# ----- Assignment task 3 ----- 
# You may have noticed that there is another network in that data file - the business network. 
#Please plot the biz network with node attributes that you’ve set above.

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




##__________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
# ----- Assignment task 4 ----- 
# Using the code already shown, plot both of the new networks. Add attributes if you wish.


##__________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
# ----- Assignment task 5 ----- 
# For the network “drug” that we created and loaded with attributes, create several different network plots, adding gender and ethnicity 
# to the graph as node attributes. Consider using a variety of colors to make your plot more informative.

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



