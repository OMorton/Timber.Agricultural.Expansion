#### Functions ####
library(tidyverse)

#### Suitability ####
suit.plot.func <- function(data, percent) {
  
  if (unique(data$suitability) == "Highly"){col <- c("#005a32", "#8c2d04")}
  if (unique(data$suitability) == "Moderately"){col <- c("#238b45", "#cc4c02")}
  if (unique(data$suitability) == "Marginal"){col <- c("#41ab5d", "#ec7014")}
  if (unique(data$suitability) == "Unsuitable"){col <- c("#74c476", "#fe9929")}
  if (any(unique(na.omit(data$RCP)) == "rcp8.5")){lin <- "solid"} else {lin <- "dashed"}
  if (percent == TRUE) {data$area.ha = (data$area.ha/data$total.ha)*100}
  if (percent == TRUE) {y.label = "% of land cover"} else {y.label = "Area of land cover (ha)"}
  
  ggplot(data, aes(time, area.ha, colour = land.cover, group = land.cover)) +
    geom_line(linetype = lin) +
    geom_point(size = 3) +
    scale_colour_manual(values = col) +
    #scale_x_discrete(labels = c("Historic", "2010-2039", "2040-2069","2070-2099")) +
    ylab(y.label) +
    xlab("Time") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45), legend.position = "none")
}


#### Cultability ####
cult.plot.func <- function(data, percent) {
  if (any(unique(na.omit(data$RCP)) == "rcp8.5")){lin <- "solid"} else {lin <- "dashed"}
  if (percent == TRUE) {data$cult.area.ha = (data$cult.area.ha/data$total.ha)*100}
  if (percent == TRUE) {y.label = "% of land cover"} else {y.label = "Area of land cover (ha)"}
  
  ggplot(data, aes(time, cult.area.ha, colour = land.cover, group = land.cover)) +
    geom_line(linetype = lin) +
    geom_point(size = 3) +
    scale_colour_manual(values = c("#005a32", "#8c2d04")) +
    #scale_x_discrete(labels = c("Historic", "2010-2039", "2040-2069","2070-2099")) +
    ylab(y.label) +
    xlab("Time") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45), legend.position = "none")
}

#### Agri Cultability ####
agri.cult.plot.func <- function(data, percent) {
  if (any(unique(na.omit(data$RCP)) == "rcp8.5")){lin <- "solid"} else {lin <- "dashed"}
  if (percent == TRUE) {data$cult.area.ha = (data$cult.area.ha/data$total.ha)*100}
  if (percent == TRUE) {y.label = "% of land cover"} else {y.label = "Area of land cover (ha)"}
  
  ggplot(data, aes(time, cult.area.ha, colour = land.cover, group = land.cover)) +
    geom_line(linetype = lin) +
    geom_point(size = 3) +
    scale_colour_manual(values = c("#005a32")) +
    #scale_x_discrete(labels = c("Historic", "2010-2039", "2040-2069","2070-2099")) +
    ylab(y.label) +
    xlab("Time") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45), legend.position = "none")
}
#### Agri suitability ####
agri.plot.func <- function(data, percent) {
  
  if (unique(data$suitability) == "Highly"){col <- c("#005a32")}
  if (unique(data$suitability) == "Moderately"){col <- c("#238b45")}
  if (unique(data$suitability) == "Marginal"){col <- c("#41ab5d")}
  if (unique(data$suitability) == "Unsuitable"){col <- c("#74c476")}
  if (any(unique(na.omit(data$RCP)) == "rcp8.5")){lin <- "solid"} else {lin <- "dashed"}
  if (percent == TRUE) {data$area.ha = (data$area.ha/data$total.ha)*100}
  if (percent == TRUE) {y.label = "% of land cover"} else {y.label = "Area of land cover (ha)"}
  
  ggplot(data, aes(time, area.ha, colour = land.cover, group = land.cover)) +
    geom_line(linetype = lin) +
    geom_point(size = 3) +
    scale_colour_manual(values = col) +
    #scale_x_discrete(labels = c("Historic", "2010-2039", "2040-2069","2070-2099")) +
    ylab(y.label) +
    xlab("Time") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45), legend.position = "none")
}


#### Fast mean ####
#https://stackoverflow.com/questions/10397574/efficiently-compute-mean-and-standard-deviation-from-a-frequency-table

fastmean <- function(dat) {
  with(dat, sum(area*value)/sum(area)) 
}

#### Fast SD ####
#https://stackoverflow.com/questions/10397574/efficiently-compute-mean-and-standard-deviation-from-a-frequency-table
fastSD <- function(dat) {
  mu <- fastmean(dat)
  with(dat, sqrt(sum(area*(value-mu)^2)/(sum(area)-1) ) )
}
