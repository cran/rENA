## ----setup, include=F---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=F, warning=F, paged.print=T--------------------------------------
#library(rENA)

## ----echo=F, message=F, warning=F---------------------------------------------
library(plotly)
library(rENA)
data(RS.data)

## ----echo=F-------------------------------------------------------------------
units = RS.data[,c("Condition","UserName")]
conversation = RS.data[,c("Condition","GroupName")]
meta = RS.data[,c("CONFIDENCE.Change","CONFIDENCE.Pre","CONFIDENCE.Post","C.Change")]

codeCols = c(
  'Data','Technical.Constraints','Performance.Parameters',
  'Client.and.Consultant.Requests','Design.Reasoning','Collaboration'
)
codes = RS.data[,codeCols]

accum = ena.accumulate.data(
  units = units,
  conversation = conversation,
  codes = codes,
  metadata = meta,
  window.size.back = 4
)

## -----------------------------------------------------------------------------
## Save references to the two vectors for easier re-use
first.game = accum$meta.data$Condition == "FirstGame"
second.game = accum$meta.data$Condition == "SecondGame"

rotation.params = list(
  FirstGame = first.game, 
  SecondGame = second.game
)

setMeansRotated = ena.make.set(
  enadata = accum,                   # The previously run accumulation above
  rotation.by = ena.rotate.by.mean,  # Function provided by rENA
  rotation.params = rotation.params  # The defined paremeters for rotation
)

first.points =  as.matrix(setMeansRotated$points$Condition$FirstGame)
second.points = as.matrix(setMeansRotated$points$Condition$SecondGame)
plot.rotated = ena.plot(setMeansRotated,  title = "Mean Rotation", scale.to = "p") %>%
                  ena.plot.points(points = first.points,  colors = c("red")) %>% 
                  ena.plot.points(points = second.points, colors = c("blue")) %>%
                  ena.plot.group(point = first.points, colors =c("red"), 
                                 confidence.interval = "box") %>%
                  ena.plot.group(point = second.points, colors =c("blue"), 
                                 confidence.interval = "box")

plot.rotated$plot

