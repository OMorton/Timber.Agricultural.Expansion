library(terra)
library(tidyterra)
library(tidyverse)
library(rnaturalearth)
options(scipen = 999)

source("Functions.r")

#### Country classified area ####
## Get country outlines
#world <- ne_download(scale = "large", returnclass = 'sf')
layers <- list.files("Data/CurtisLayers/", full.names = TRUE)[1:14]
timber.countries <- c("USA", "Russia", "China", "Brazil", "Canada")
timber.codes <- c("USA", "RUS", "CHN", "BRA", "CAN")

c.lyr.dat <- data.frame()

for (i in 1:length(timber.countries)) {
  
  country.i <- timber.countries[i]
  code.i <- timber.codes[i]
  
  country.border <- vect(paste0("Data/GADM/GADM_",country.i,"/gadm41_", code.i, "_0.shp" ))
  cat("Working on ", country.i, ": ", i, "out of", length(timber.countries), '\n')
  
  for (j in 1:length(layers)) {
    lyr <- layers[j]
    cat(lyr, '\n', j, "out of", length(layers), '\n')
    
    if (grepl(pattern = "current", lyr) == TRUE){time <- "Current"}
    if (grepl(pattern = "2010", lyr) == TRUE){time <- "2010-2039"}
    if (grepl(pattern = "2040", lyr) == TRUE){time <- "2040-2069"}
    if (grepl(pattern = "2070", lyr) == TRUE){time <- "2070-2099"}
    if (grepl(pattern = "rcp8p5", lyr) == TRUE){rcp <- "rcp8.5"}
    if (grepl(pattern = "rcp2p6", lyr) == TRUE){rcp <- "rcp2.6"}
    if (grepl(pattern = "current", lyr) == TRUE){rcp <- NA}
    if (grepl(pattern = "no.forestry", lyr) == TRUE){land.cover <- "Non-forestry"} else {
      land.cover <- "Forestry"}
      
      c.extent <- terra::ext(country.border)
      mask.c.area <- rast(lyr) %>% crop(., c.extent) %>%mask(., country.border)  
                          
      tot <- mask.c.area %>% expanse(unit = "ha")
      mask.ex <- mask.c.area %>% expanse(byValue = TRUE, unit = "ha")
                                              
      c.lyr.add <- mask.ex %>% mutate(total.ha = tot$area, RCP = rcp, time = time, land.cover, country = country.i)
      
      c.lyr.dat <- rbind(c.lyr.dat, c.lyr.add)                    
  }
  write.csv(c.lyr.dat, paste0("Data/CountryArea/temp.out.", i, ".csv"))
  
}
c.lyr.dat <- c.lyr.dat %>% mutate(suitability = case_when(value == 1 ~ "Unsuitable",
                                                          value == 2 ~ "Marginal",
                                                          value == 3 ~ "Moderately",
                                                          value == 4 ~ "Highly")) %>%
  rename("area.ha" = "area") %>% 
  select(-c(layer, value))

#write.csv(c.lyr.dat, "Data/CountryArea/Timber.top5.suitability.csv")
c.lyr.dat <- read.csv("Data/CountryArea/Timber.top5.suitability.csv")




#### Country improving area ####
layers <- list.files("Data/CurtisLayers/", full.names = TRUE)[1:14]
timber.countries <- c("USA", "Russia", "China", "Brazil", "Canada")
timber.codes <- c("USA", "RUS", "CHN", "BRA", "CAN")

diff.dat <- data.frame()

for (i in 1:length(timber.countries)) {
  
  country.i <- timber.countries[i]
  code.i <- timber.codes[i]
  
  country.border <- vect(paste0("Data/GADM/GADM_",country.i,"/gadm41_", code.i, "_0.shp" ))
  cat("Working on ", country.i, ": ", i, "out of", length(timber.countries), '\n')
  
  c.extent <- terra::ext(country.border)
  
  cat("Masking", '\n')
  
  mask.c.area.curr <- rast("Data/CurtisLayers/Raw_suitability_scores/curtis.forestry.current.ag.suitability.tif") %>% 
    crop(., c.extent) %>%mask(., country.border)  
  
  mask.c.area.26 <- rast("Data/CurtisLayers/Raw_suitability_scores/curtis.forestry.2070.2099.rcp2p6.ag.suitability.tif") %>% 
    crop(., c.extent) %>%mask(., country.border) 
  
  mask.c.area.85 <- rast("Data/CurtisLayers/Raw_suitability_scores/curtis.forestry.2070.2099.rcp8p5.ag.suitability.tif") %>% 
    crop(., c.extent) %>%mask(., country.border) 
  
  cat("Differencing", '\n')
  
  diff.85 <- mask.c.area.85 - mask.c.area.curr
  diff.26 <- mask.c.area.26 - mask.c.area.curr
  
  m <- c(0, 0, 1,
         1, 100, 2,
         -100, -1, 3)
  chng.matr <- matrix(m, ncol=3, byrow=TRUE)
  
  cat("Classifying", '\n')
  
  ## NA for closed at both sides
  diff.85.cl <- classify(diff.85, chng.matr, right = NA)
  diff.26.cl <- classify(diff.26, chng.matr, right = NA)
  
  cat("Expansing", '\n')
  
  diff.85.cl <- expanse(diff.85.cl, byValue = TRUE, unit = "ha")
  diff.26.cl <- expanse(diff.26.cl, byValue = TRUE, unit = "ha")
  diff.85.cl$rcp <- "rcp8p5"
  diff.26.cl$rcp <- "rcp2p6"
  diff.85.cl$country <- country.i
  diff.26.cl$country <- country.i
  
  diff.add <- rbind(diff.26.cl, diff.85.cl) %>% mutate(change = case_when(value == 1 ~ "No change",
                                                                          value == 2 ~ "Increase",
                                                                          value == 3 ~ "Decrease"))
  
  diff.dat <- rbind(diff.dat, diff.add)       
  
}

write.csv(diff.dat, "Data/CountryArea/Timber.top5.changes.classified.csv")

diff.dat %>% group_by(rcp, country) %>% tally(area)
diff.dat %>% group_by(rcp) %>% tally(area)

#### All countries suitablility ####
layers <- list.files("Data/CurtisLayers/", full.names = TRUE)[1:14]

world <- sf::st_read("Data/GADMworld/gadm_410-levels.gpkg", layer = "ADM_0")

all.c.suit.dat <- data.frame()

i <- 6
for (i in 1:nrow(world)) {
  
  country.i.lyr <- world[i,]
  country.i.id <- country.i.lyr$COUNTRY
  country.i.gid <- country.i.lyr$GID_0
  
  #country.i.cont <- country.i.lyr$continent
  #country.i.reg <- country.i.lyr$region.wb
  
  cat("Working on ", country.i.id, ": ", i, "out of", nrow(world), '\n')
  
  c.extent <- terra::ext(vect(country.i.lyr$geom))
  country.border <- vect(country.i.lyr$geom)
  
  for (j in 1:length(layers)) {
    lyr <- layers[j]
    cat(lyr, '\n', j, "out of", length(layers), '\n')
    
    if (grepl(pattern = "current", lyr) == TRUE){time <- "Current"}
    if (grepl(pattern = "2010", lyr) == TRUE){time <- "2010-2039"}
    if (grepl(pattern = "2040", lyr) == TRUE){time <- "2040-2069"}
    if (grepl(pattern = "2070", lyr) == TRUE){time <- "2070-2099"}
    if (grepl(pattern = "rcp8p5", lyr) == TRUE){rcp <- "rcp8.5"}
    if (grepl(pattern = "rcp2p6", lyr) == TRUE){rcp <- "rcp2.6"}
    if (grepl(pattern = "current", lyr) == TRUE){rcp <- NA}

  
  cat("Masking", '\n')
  
  mask.c.area.curr <- rast(lyr) %>% 
    crop(., c.extent) %>%mask(., country.border)  
  
  cat("Expansing", '\n')
  
  c.suit.ex <- expanse(mask.c.area.curr, byValue = TRUE, unit = "ha")

  if(nrow(c.suit.ex) == 0) {c.suit.ex <- data.frame(layer = "NO.FOREST", value = "NO.FOREST", area = "NO.FOREST")}
  
  c.suit.i.add <- c.suit.ex %>% 
    mutate(country = country.i.id, country.gid = country.i.gid, rcp = rcp, time = time,
           suitability = case_when(value == 0 ~ "Unsuitable", value > 0 & value < 33 ~ "Marginal",
                              value > 32 & value < 75 ~ "Moderately", value > 74 ~ "Highly",
                              value == "NO.FOREST" ~ "NO.FOREST"))
  
  all.c.suit.dat <- rbind(all.c.suit.dat, c.suit.i.add)       
  }
  }



write.csv(all.c.suit.dat, "Data/CountryArea/all.country.suitability.area.raw.csv")

#### All countries change ####

world <- sf::st_read("Data/GADMworld/gadm_410-levels.gpkg", layer = "ADM_0")
world.l <- sf::st_layers("Data/GADMworld/gadm_410-levels.gpkg")

world <- rnaturalearth::ne_countries(returnclass = "sf")
c.world <- world[15,]

c.world$name
terra::ext(vect(c.world$geometry))


c.lyr.dat[1,]
plot(world)

all.c.dat <- data.frame()

i <- 6
for (i in 1:nrow(world)) {
  
  country.i.lyr <- world[i,]
  country.i.id <- country.i.lyr$COUNTRY
  country.i.gid <- country.i.lyr$GID_0
  
  #country.i.cont <- country.i.lyr$continent
  #country.i.reg <- country.i.lyr$region.wb
  
  cat("Working on ", country.i.id, ": ", i, "out of", nrow(world), '\n')
  
  c.extent <- terra::ext(vect(country.i.lyr$geom))
  country.border <- vect(country.i.lyr$geom)
  
  cat("Masking", '\n')
  
  mask.c.area.curr <- rast("Data/CurtisLayers/Raw_suitability_scores/curtis.forestry.current.ag.suitability.tif") %>% 
    crop(., c.extent) %>%mask(., country.border)  
  
  mask.c.area.26 <- rast("Data/CurtisLayers/Raw_suitability_scores/curtis.forestry.2070.2099.rcp2p6.ag.suitability.tif") %>% 
    crop(., c.extent) %>%mask(., country.border) 
  
  mask.c.area.85 <- rast("Data/CurtisLayers/Raw_suitability_scores/curtis.forestry.2070.2099.rcp8p5.ag.suitability.tif") %>% 
    crop(., c.extent) %>%mask(., country.border) 
    
    cat("Differencing", '\n')
    
    diff.85 <- mask.c.area.85 - mask.c.area.curr
    diff.26 <- mask.c.area.26 - mask.c.area.curr
    
    cat("Expansing", '\n')
    
    diff.85.ex <- expanse(diff.85, byValue = TRUE, unit = "ha")
    diff.26.ex <- expanse(diff.26, byValue = TRUE, unit = "ha")
    
    if(nrow(diff.26.ex) == 0) {diff.26.ex <- data.frame(layer = "NO.FOREST", value = "NO.FOREST", area = "NO.FOREST", rcp = "rcp2p6")}
    if(nrow(diff.85.ex) == 0) {diff.85.ex <- data.frame(layer = "NO.FOREST", value = "NO.FOREST", area = "NO.FOREST", rcp = "rcp8p5")}
    if(nrow(diff.85.ex) != 0) {diff.85.ex$rcp <- "rcp8p5"}
    if(nrow(diff.26.ex) != 0) {diff.26.ex$rcp <- "rcp2p6"}
    
    c.i.add <- rbind(diff.26.ex, diff.85.ex) %>% 
      mutate(country = country.i.id, country.gid = country.i.gid,
             change = case_when(value > 0 ~ "Increase", value < 0 ~ "Decrease", value == 0 ~ "No change",
                                value == "NO.FOREST" ~ "NO.FOREST"))
    
    all.c.dat <- rbind(all.c.dat, c.i.add)       
  }

all.c.dat.sum <- all.c.dat %>% filter(area != "NO.FOREST") %>% 
  group_by(rcp, country, country.gid, change) %>% summarise(area = sum(as.numeric(as.character(area))))

write.csv(all.c.dat, "Data/CountryArea/all.country.change.area.2070.2099.raw.csv")
write.csv(all.c.dat.sum, "Data/CountryArea/all.country.change.area.2070.2099.sum.csv")

#### Country frontier area #####
front.layers <- list.files("Data/Agri.Frontiers/", full.names = TRUE)
timber.countries <- c("USA", "Russia", "China", "Brazil", "Canada")
timber.codes <- c("USA", "RUS", "CHN", "BRA", "CAN")

front.dat <- data.frame()

for (i in 1:length(timber.countries)) {
  
  country.i <- timber.countries[i]
  code.i <- timber.codes[i]
  
  country.border <- vect(paste0("Data/GADM/GADM_",country.i,"/gadm41_", code.i, "_0.shp" ))
  cat("Working on ", country.i, ": ", i, "out of", length(timber.countries), '\n')
  
  for (j in 1:length(front.layers)) {
    lyr <- front.layers[j]
    cat(lyr, '\n', j, "out of", length(front.layers), '\n')
    
    if (grepl(pattern = "2040", lyr) == TRUE){time <- "2040-2069"}
    if (grepl(pattern = "2070", lyr) == TRUE){time <- "2070-2099"}
    if (grepl(pattern = "rcp8p5", lyr) == TRUE){rcp <- "rcp8.5"}
    if (grepl(pattern = "rcp2p6", lyr) == TRUE){rcp <- "rcp2.6"}
    
    c.extent <- terra::ext(country.border)
    
    cat("Masking", '\n')
    
    front.c.area <- rast(lyr) %>% 
      crop(., c.extent) %>%mask(., country.border)  
    
    cat("Expansing", '\n')
    
    front.c.area.ex <- expanse(front.c.area, byValue = TRUE, unit = "ha")
    
    front.c.area.ex$rcp <- rcp
    front.c.area.ex$time <- time
    front.c.area.ex$country <- country.i
    
    front.dat <- rbind(front.dat, front.c.area.ex)       
    
  }
}
write.csv(front.dat, "Data/CountryArea/Timber.top5.frontier.area.raw.csv")

front.dat.sum <- front.dat %>% group_by(rcp,country, time) %>% summarise(area = sum(area))
write.csv(front.dat.sum, "Data/CountryArea/Timber.top5.frontier.area.sum.csv")



#### Continent level ####

continent.ls <- c("North America", "South America", "Africa", "Europe", "Asia", "Oceania")
cont.lyr.dat <- data.frame()

for (i in 1:length(continent.ls)) {
  
  continent.i <- continent.ls[i]
  continent.border <- sf::st_as_sf(ne_countries(scale = "large", continent = continent.i))
  
  cat("Working on ", continent.i, ": ", i, "out of", length(continent.ls), '\n')
  
  for (j in 1:length(layers)) {
    lyr <- layers[j]
    cat(lyr, '\n', j, "out of", length(layers), '\n')
    
    if (grepl(pattern = "current", lyr) == TRUE){time <- "Current"}
    if (grepl(pattern = "2010", lyr) == TRUE){time <- "2010-2039"}
    if (grepl(pattern = "2040", lyr) == TRUE){time <- "2040-2069"}
    if (grepl(pattern = "2070", lyr) == TRUE){time <- "2070-2099"}
    if (grepl(pattern = "rcp8p5", lyr) == TRUE){rcp <- "rcp8.5"}
    if (grepl(pattern = "rcp2p6", lyr) == TRUE){rcp <- "rcp2.6"}
    if (grepl(pattern = "current", lyr) == TRUE){rcp <- NA}
    if (grepl(pattern = "no.forestry", lyr) == TRUE){land.cover <- "Non-forestry"} else {
      land.cover <- "Forestry"}
    
    cont.extent <- terra::ext(continent.border)
    mask.cont.area <- rast(lyr) %>% crop(., cont.extent) %>%mask(., vect(continent.border))  
    
    tot <- mask.cont.area %>% expanse(unit = "ha")
    Unsuitable <- mask.cont.area %>% ifel(. != 1, NA, .) %>% expanse(unit = "ha")
    Marginal <- mask.cont.area %>% ifel(. != 2, NA, .) %>% expanse(unit = "ha")
    Moderately <- mask.cont.area %>% ifel(. != 3, NA, .) %>% expanse(unit = "ha")
    Highly <- mask.cont.area %>% ifel(. != 4, NA, .) %>% expanse(unit = "ha")
    
    cont.lyr.add <- data.frame(suitability = c("Unsuitable", "Marginal", "Moderately", "Highly"),
                            area.ha = c(Unsuitable$area, Marginal$area, Moderately$area, Highly$area),
                            total.ha = tot$area, RCP = rcp, time = time, land.cover, continent = continent.i)
    
    cont.lyr.dat <- rbind(cont.lyr.dat, cont.lyr.add)                    
  }
  write.csv(cont.lyr.dat, paste0("Data/ContinentArea/temp.out.", i, ".csv"))
  
}

#write.csv(cont.lyr.dat, "Data/ContinentArea/ContinentSuitability.csv")
cont.lyr.dat <- read.csv("Data/ContinentArea/ContinentSuitability.csv")




