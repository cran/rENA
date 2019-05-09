## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=F, warning=F, paged.print=T---------------------------------
library(rENA)
data(RS.data)

## ----echo=F, message=F, warning=F----------------------------------------
library(plotly)

## ------------------------------------------------------------------------

units = RS.data[,c("Condition","UserName")]
head(units)

conversation = RS.data[,c("Condition","GroupName")]
head(conversation)

codeCols = c(
  'Data','Technical.Constraints','Performance.Parameters',
  'Client.and.Consultant.Requests','Design.Reasoning','Collaboration'
)
codes = RS.data[,codeCols]
head(codes)

# optional
meta = RS.data[,c("CONFIDENCE.Change",
                  "CONFIDENCE.Pre","CONFIDENCE.Post","C.Change")]
head(meta)

## ------------------------------------------------------------------------
accum = ena.accumulate.data(
  units = units,
  conversation = conversation,
  codes = codes,
  metadata = meta,
  window.size.back = 4
)

### adjacency.vectors: Each Unit's Co-Occurrence Accumulation
#head(accum$adjacency.vectors)

### adjacency.matrix: Columns representing co-occurred 
### codes in the adjacency.vector
#head(accum$adjacency.matrix)

## ------------------------------------------------------------------------
set = ena.make.set(
  enadata = accum
)

### The location in space for each unit.  Units are rows, columns are each 
### dimension in the high-dimensional space.
#head(set$points.rotated)

### The positiona of each code in the high-dimensional space
#head(set$node.positions)

### The weight of each connection. Units are rows, columns the co-occurrence
#head(set$line.weights)

## ----Create Logical Vectors and Subset Two Groups------------------------
### Make a reference to the units for easier access
unitNames = set$enadata$units

### Logical vector representing which rows will be FirstGame
first.game = unitNames$Condition == "FirstGame"

### Logical vector representing which rows will be SecondGame
second.game = unitNames$Condition == "SecondGame"

## ----echo=TRUE-----------------------------------------------------------
### Subset rotated points for the first condition
first.game.points = set$points.rotated[first.game,]

### Subset rotated points for the second condition
second.game.points = set$points.rotated[second.game,]

plot = ena.plot(set, scale.to = "points", title = "Groups of Units")
plot = ena.plot.points(plot, points = first.game.points, confidence.interval = "box", colors = c("red"))
plot = ena.plot.points(plot, points = second.game.points, confidence.interval = "box", colors = c("blue"))
plot$plot

## ------------------------------------------------------------------------
### Using the same plot object above, we will be able to plot the means 
### alongside their corresponding units.
plot = ena.plot(set, scale.to = "points", title = "Groups and Means")
plot = ena.plot.points(plot, points = first.game.points, 
                       confidence.interval = "box", colors = c("red"))
plot = ena.plot.points(plot, points = second.game.points, 
                       confidence.interval = "box", colors = c("blue"))
plot = ena.plot.group(plot, point = first.game.points, 
                      colors =c("red"), confidence.interval = "box")
plot = ena.plot.group(plot, point = second.game.points, 
                      colors =c("blue"), confidence.interval = "box")
plot$plot

## ------------------------------------------------------------------------
### Subset lineweights for FirstGame and Calculate the colMeans
first.game.lineweights = set$line.weights[first.game,]

### Subset lineweights for SecondGame and Calculate the colMeans
second.game.lineweights = set$line.weights[second.game,]

## ----Calculate Means and Their Differences of Two Groups-----------------
first.game.mean = as.vector(colMeans(first.game.lineweights))
second.game.mean = as.vector(colMeans(second.game.lineweights))

### Subtract the two sets of means, resulting in a vector with negative values
### indicatinag a stronger connection with the SecondGame, and positive values
### a stronger FirstGame connection
subtracted.mean = first.game.mean - second.game.mean

# View the first 5 elements to see the substraction
head(first.game.mean, 5)
head(second.game.mean, 5)
head(subtracted.mean, 5)

## ----Plot Network of Units in FirstGame----------------------------------
#Plot subtracted network only
plot.first = ena.plot(set, title = "FirstGame")
plot.first = ena.plot.network(plot.first, network = first.game.mean)
plot.first$plot

## ----Plot Network of Units in SecondGame---------------------------------
plot.second = ena.plot(set, title = "SecondGame")
plot.second = ena.plot.network(plot.second, network = second.game.mean, colors = c("blue"))
plot.second$plot

## ----Plot a Subtracted Network-------------------------------------------
plot.sub = ena.plot(set, title = "Subtracted")
plot.sub = ena.plot.network(plot.sub, network = subtracted.mean)
plot.sub$plot

## ------------------------------------------------------------------------
library(magrittr)
library(scales)

# Scale the nodes to match that of the network, for better viewing
point.max = max(first.game.points, second.game.points)
first.game.scaled = scales::rescale(first.game.points, 
                                    c(0,max(set$node.positions)), c(0,point.max))
second.game.scaled = scales::rescale(second.game.points, 
                                     c(0,max(set$node.positions)), c(0,point.max))

plot = ena.plot(set, title = "Plot with Units and Network") %>% 
          ena.plot.points(points = first.game.scaled, colors = c("red")) %>% 
          ena.plot.points(points = second.game.scaled, colors = c("blue")) %>% 
          ena.plot.group(point = first.game.scaled, colors =c("red"), 
                         confidence.interval = "box") %>% 
          ena.plot.group(point = second.game.scaled, colors =c("blue"), 
                         confidence.interval = "box") %>%
          ena.plot.network(network = subtracted.mean)

plot$plot

## ------------------------------------------------------------------------
# List of codes to hide
nodes.to.hide = c("Data", "Client.and.Consultant.Requests")

# Create a logical vector representing which codes to show
codes.to.show = !(set$enadata$adjacency.matrix[1,] %in%  nodes.to.hide) & 
                  !(set$enadata$adjacency.matrix[2,] %in% nodes.to.hide)

nodes.to.show = !rownames(set$node.positions) %in% nodes.to.hide
plot.subset.weights = ena.plot(set) %>% 
                        ena.plot.network(
                          network = subtracted.mean[codes.to.show], 
                          node.positions = set$node.positions[nodes.to.show,]
                        )

plot.subset.weights$plot

