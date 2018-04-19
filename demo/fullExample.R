data(RS.data)

codeNames = c('Data','Technical.Constraints','Performance.Parameters','Client.and.Consultant.Requests','Design.Reasoning','Collaboration');

accum = rENA::ena.accumulate.data(
  units = RS.data[,c("Condition","UserName")],
  conversation = RS.data[,c("Condition","GroupName")],
  metadata = RS.data[,c("CONFIDENCE.Change","CONFIDENCE.Pre","CONFIDENCE.Post","C.Change")],
  codes = RS.data[,codeNames],
  model = "EndPoint",
  window.size.back = 4
);
set = ena.make.set(
  enadata = accum,
  rotation.by = ena.rotate.by.mean,
  rotation.params = list(accum$metadata$Condition=="FirstGame", accum$metadata$Condition=="SecondGame")
);
accum2 = ena.accumulate.data.file(
  file = RS.data,
  window.size.back = 4,
  units.by = make.names(c("Condition","UserName")),
  units.used= c('FirstGame.brandon l','FirstGame.steven z','FirstGame.connor f','FirstGame.amalia x','FirstGame.joseph k','FirstGame.mitchell h','FirstGame.robert z','FirstGame.amirah u','FirstGame.cameron k','FirstGame.fletcher l','FirstGame.joseph h','FirstGame.margaret n','FirstGame.amelia n','FirstGame.tiffany x','FirstGame.carl b','FirstGame.kevin g','FirstGame.akash v','FirstGame.alexander b','FirstGame.jordan l','FirstGame.peter h','FirstGame.arden f','FirstGame.jimmy i','FirstGame.peter','FirstGame.luis','FirstGame.christian x','FirstGame.devin','SecondGame.derek v','SecondGame.cormick u','SecondGame.christina b','SecondGame.nicholas n','SecondGame.keegan q','SecondGame.nicholas l','SecondGame.cameron i','SecondGame.daniel t','SecondGame.jackson p','SecondGame.samuel o','SecondGame.brandon f','SecondGame.casey f','SecondGame.shane t','SecondGame.luke u','SecondGame.abigail z','SecondGame.nathan d','SecondGame.justin y','SecondGame.brent p','SecondGame.madeline g','SecondGame.kiana k','SecondGame.ruzhen e','SecondGame.caitlyn y'),
  model = "EndPoint",
  conversations.by = make.names(c("Condition","GroupName")),
  codes = make.names(codeNames)
)
set2 = rENA::ena.make.set(
  enadata = accum
  #,rotation.by = rENA:::ena.rotate.by.mean,
  #rotation.params = list(accum$metadata$Condition=="FirstGame", accum$metadata$Condition=="SecondGame")
);

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
plot1 = rENA::ena.plot(set)
plot1 = rENA::ena.plot.network(plot1, network = subtracted.network)

#plot means only
plot2 = rENA::ena.plot(set)
plot2 = rENA::ena.plot.group(plot2, second.game.points, labels = "SecondGame", colors  = "blue", confidence.interval = "box")
plot2 = rENA::ena.plot.group(plot2, first.game.points, labels = "FirstGame", colors = "red", confidence.interval = "box")

#plot both
plot3 = rENA::ena.plot(set)
plot3 = rENA::ena.plot.network(plot3, network = subtracted.network)
plot3 = rENA::ena.plot.group(plot3, first.game.points, labels = "FirstGame", colors = "red", confidence.interval = "box")
plot3 = rENA::ena.plot.group(plot3, second.game.points, labels = "SecondGame", colors  = "blue", confidence.interval = "box")

dim.by.activity = cbind(
 set$points.rotated[,1],
 set$enadata$trajectories$step$ActivityNumber*.8/14-.4  #scale down to dimension 1
)

accum = ena.accumulate.data(
  units = RS.data[,c("UserName","Condition")],
  conversation = RS.data[,c("GroupName","ActivityNumber")],
  metadata = RS.data[,c("CONFIDENCE.Change","CONFIDENCE.Pre","CONFIDENCE.Post","C.Change")],
  codes = RS.data[,codeNames],
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
