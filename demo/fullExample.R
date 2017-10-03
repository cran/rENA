file <- read.csv(system.file("extdata", "rs.data.csv", package="rENA"))

codeNames = c('Data','Technical.Constraints','Performance.Parameters','Client.and.Consultant.Requests','Design.Reasoning','Collaboration');

accum = ena.accumulate.data(
  units = file[,c("UserName","Condition")],
  conversation = file[,c("Condition","GroupName")],
  metadata = file[,c("CONFIDENCE.Change","CONFIDENCE.Pre","CONFIDENCE.Post","C.Change")],
  codes = file[,codeNames],
  window.size.back = 4
);
set = ena.make.set(
  enadata = accum,
  rotation.by = rENA:::ena.rotate.by.mean,
  rotation.params = list(accum$metadata$Condition=="FirstGame", accum$metadata$Condition=="SecondGame")
)

unitNames = set$enadata$units

### Subset rotated points and plot Condition 1 Group Mean
first.game = unitNames$Condition == "FirstGame"
first.game.points = set$points.rotated[first.game,]

### Subset rotated points and plot Condition 2 Group Mean
second.game = unitNames$Condition == "SecondGame"
second.game.points = set$points.rotated[second.game,]

#first.game.mean = colMeans( first.game.points )
#second.game.mean = colMeans( second.game.points )

#first.game.ci = t.test(first.game.points, conf.level = 0.95)$conf.int
#second.game.ci = t.test(second.game.points, conf.level = 0.95)$conf.int

### get mean network plots
first.game.lineweights = set$line.weights[first.game,]
first.game.mean = colMeans(first.game.lineweights)

second.game.lineweights = set$line.weights[second.game,]
second.game.mean = colMeans(second.game.lineweights)

subtracted.network = first.game.mean - second.game.mean

#Plot subtracted network only
plot1 = ena.plot(set)
plot1 = ena.plot.network(plot1, network = subtracted.network)

#plot means only
plot2 = ena.plot(set)
plot2 = ena.plot.group(plot2, second.game.points, labels = "SecondGame", colors  = "blue", confidence.interval = "box")
plot2 = ena.plot.group(plot2, first.game.points, labels = "FirstGame", colors = "red", confidence.interval = "box")

#plot both
plot3 = ena.plot(set)
plot3 = ena.plot.network(plot3, network = subtracted.network)
plot3 = ena.plot.group(plot3, first.game.points, labels = "FirstGame", colors = "red", confidence.interval = "box")
plot3 = ena.plot.group(plot3, second.game.points, labels = "SecondGame", colors  = "blue", confidence.interval = "box")

dim.by.activity = cbind(
 set$points.rotated[,1],
 set$enadata$trajectories$step$ActivityNumber*.8/14-.4  #scale down to dimension 1
)

accum = ena.accumulate.data(
  units = file[,c("UserName","Condition")],
  conversation = file[,c("GroupName","ActivityNumber")],
  metadata = file[,c("CONFIDENCE.Change","CONFIDENCE.Pre","CONFIDENCE.Post","C.Change")],
  codes = file[,codeNames],
  window.size.back = 4,
  model = "A"
);

set = ena.make.set(accum);
plot = ena.plot(set)
plot = ena.plot.network(plot, network = subtracted.network, legend.name="Network", legend.include.edges = T)

dim.by.activity = cbind(
 set$points.rotated[,1],
 set$enadata$trajectories$step$ActivityNumber*.8/14-.4  #scale down to dimension 1
)
plot = ena.plot.trajectory(
 plot,
 points = dim.by.activity,
 names = unique(set$enadata$units$UserName),
 by = set$enadata$units$UserName
);
print(plot)
