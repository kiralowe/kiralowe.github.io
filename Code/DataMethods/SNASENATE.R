
library(readr)
library(network)
library(ggplot2)
links <- read_csv("Cosponsors_By_Bill_SENATE1.csv")  # bills 
View(links)
nodes <- read_csv("CosponsorsByLegislatorSENATE1.csv") # legislators
View(nodes)

## Examine Data ##
head(nodes)
head(links)
library(dplyr)
nodes = select(nodes, -weight)

# Some data management
nodes$ltsb=log(nodes$SPONSORED+1) # legislator file column
install.packages("BBmisc")
library(BBmisc)
nodes$weight=normalize(nodes$ltsb, method="range")
nodes$weight
nodes$ltsb
summary(nodes$weight)
nrow(nodes); length(unique(nodes$id))
nrow(links); nrow(unique(links[,c("from", "to")]))
# There are more links than unique from-to combinations
# We need to collapse somehow... 

# create a 0/1 variable GOP 
# create a 0/1 variable Dem

nodes$GOP
sum(nodes$GOP) # check number of GOP lawmakers

nodes$DEM 
sum(nodes$DEM) # check number of Dem lawmakers


# create a 0/1 variable male

nodes$MALE
sum(nodes$MALE) # check number of male

nodes$FEMALE
sum(nodes$FEMALE) # check number of female


# 0/1 variable for lose/win 2016 election

nodes$election_past
sum(nodes$election_past)


# 0/1 variable for lose/win 2018 election

nodes$election_recent
sum(nodes$election_recent)

## Turning Network into igraph object ##
library(igraph)
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
class(net)
net

# Intial Network Attributes 
# Check variable names

E(net)       # The edges of the "net" object
E(net)$date  # Edge attribute "date"
E(net)$term  # Edge attribute "term"
E(net)$sessionPeriod  # Edge attribute "sessionPeriod"
E(net)$sessionTimes  # Edge attribute "sessionTimes"
E(net)$Bill  # Edge attribute "Bill"

V(net)       # The vertices of the "net" object
V(net)$GOP   # Vertex/Node attribute "GOP"
V(net)$DEM   # Vertex/Node attribute "Dem"
V(net)$MALE # Vertex/Node attribute "male"
V(net)$COMMITTEE_RANK
V(net)$election_past # Vertex/Node attribute "2016 election"
V(net)$election_recent # Vertex/Node attribute "2018 election"


## Initial plot attempt ##

plot(net, edge.arrow.size=.4,vertex.label=NA)
svg(filename="plot01.svg")
dev.off()

# Remove multiple ties between two legislators
net <- simplify(net, remove.multiple = T, remove.loops = T) 
plot(net, edge.arrow.size=.001,vertex.label=NA,vertex.size=nodes$ltsb)
dev.off()

# Labeling the nodes by party
# Dem members = blue
# GOP = red

V(net)$color=V(net)$GOP
V(net)$color
V(net)$color=gsub("1", "red", V(net)$color)
V(net)$color=gsub("0", "blue", V(net)$color)

plot(net, vertex.color=V(net)$color, edge.arrow.size=.1,vertex.label=NA,edge.width=0.1, vertex.size=nodes$ltsb)

# with ID number
plot(net, vertex.color=V(net)$color, edge.arrow.size=.1,vertex.label=V(net)$id,edge.width=0.05, vertex.size=nodes$ltsb)

# with size relative to committees 
plot(net, vertex.color=V(net)$color, edge.arrow.size=.05,vertex.label=NA, V(net)$size <- V(net)$COMMITTEE, edge.width=0.1, vertex.size=nodes$ltsb)

dev.off()
library(ggplot2)

# Fruchterman Reingold Layout #
layFR <- layout.fruchterman.reingold(net)
layout=1

plot(net, vertex.color=V(net)$color, vertex.label=nodes$id, 
        edge.arrow.size=.05, edge.color="gray70", 
        vertex.label.dist=0.75,vertex.label.font=1,
        vertex.label.cex=.5,
        vertex.label.family="STHeiti",layout=layFR,edge.width=0.05, 
        vertex.size=nodes$ltsb, main="")
dev.off()

# change to color by 2016 election results

V(net)$color=V(net)$election_past
V(net)$color
V(net)$color=gsub("1", "turquoise1", V(net)$color) #reelected is light
V(net)$color=gsub("0", "darkslategray4", V(net)$color)  #not reelected is dark

plot(net, vertex.color=V(net)$color, vertex.label=nodes$id, 
     edge.arrow.size=.05, edge.color="gray70", 
     vertex.label.dist=0.75,vertex.label.font=1,
     vertex.label.cex=.5,
     vertex.label.family="STHeiti",layout=layFR,edge.width=0.05, 
     vertex.size=nodes$ltsb, main="")

ggsave("igraphFR.pdf", dpi=1200)
dev.off()

#change color to 2018 election results

V(net)$color=V(net)$election_recent
V(net)$color
V(net)$color=gsub("1", "turquoise1", V(net)$color) #reelected is light
V(net)$color=gsub("0", "darkslategray4", V(net)$color)  #not reelected is dark

plot(net, vertex.color=V(net)$color, vertex.label=nodes$id, 
     edge.arrow.size=.05, edge.color="gray70", 
     vertex.label.dist=0.75,vertex.label.font=1,
     vertex.label.cex=.5,
     vertex.label.family="STHeiti",layout=layFR,edge.width=0.05, 
     vertex.size=nodes$ltsb, main="")

## Plot layouts ##

#Back to red/blue party colors

V(net)$color=V(net)$GOP
V(net)$color
V(net)$color=gsub("1", "red", V(net)$color)
V(net)$color=gsub("0", "blue", V(net)$color)

# Spherical Layout 
# laySphere <- layout_on_sphere(net)
# layout=1
# png(filename="plot04.png")
# plot(net, vertex.color=V(net)$color, vertex.label=NA, edge.arrow.size=.4, edge.color="gray70", 
          #layout=laySphere,edge.width=0.05,vertex.size=nodes$ltsb,main="Spherical Layout")
# dev.off()

# LGL Algorithm # 
png(filename="plot05.png")
plot(net, vertex.color=V(net)$color, vertex.label=NA, edge.arrow.size=.01, edge.color="gray70",edge.width=0.1, layout=layout_with_lgl,vertex.size=nodes$ltsb,main="LGL")
dev.off()

# Kamada Kawai # 
plot(net, vertex.color=V(net)$color, vertex.label=NA, edge.arrow.size=.1, edge.color="gray70", layout=layout_with_kk,edge.width=0.1,vertex.size=nodes$ltsb,main="KK")
dev.off()

# Random layout #
# Not as helpful for our purposes
#plot(net, vertex.color=V(net)$color, vertex.label=NA, edge.arrow.size=.1, edge.color="gray70", layout=layout_randomly,edge.width=0.1,vertex.size=nodes$ltsb)
#dev.off()


