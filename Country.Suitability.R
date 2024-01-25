library(terra)
library(tidyterra)
library(tidyverse)
library(rnaturalearth)
options(scipen = 999)

source("Functions.r")

#### Country level ####
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
      Unsuitable <- mask.c.area %>% ifel(. != 1, NA, .) %>% expanse(unit = "ha")
      Marginal <- mask.c.area %>% ifel(. != 2, NA, .) %>% expanse(unit = "ha")
      Moderately <- mask.c.area %>% ifel(. != 3, NA, .) %>% expanse(unit = "ha")
      Highly <- mask.c.area %>% ifel(. != 4, NA, .) %>% expanse(unit = "ha")
                                              
      c.lyr.add <- data.frame(suitability = c("Unsuitable", "Marginal", "Moderately", "Highly"),
                            area.ha = c(Unsuitable$area, Marginal$area, Moderately$area, Highly$area),
                            total.ha = tot$area, RCP = rcp, time = time, land.cover, country = country.i)
      
      c.lyr.dat <- rbind(c.lyr.dat, c.lyr.add)                    
  }
  write.csv(c.lyr.dat, paste0("Data/CountryArea/temp.out.", i, ".csv"))
  
}
#write.csv(c.lyr.dat, "Data/CountryArea/Timber.top5.suitability.csv")
c.lyr.dat <- read.csv("Data/CountryArea/Timber.top5.suitability.csv")




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


## make ordered
c.lyr.dat <- c.lyr.dat %>%
  mutate(time = ordered(time, levels = c("Current", "2010-2039", "2040-2069","2070-2099"))) %>%
  filter(land.cover == "Forestry")

#### Plot country suitability area ####
c.rcp26 <- c.lyr.dat %>% filter(RCP %in% c("rcp2.6",NA), land.cover == "Forestry") %>% 
  mutate(country = ordered(country, levels = c("USA", "Russia", "China","Brazil", "Canada")))
c.rcp85 <- c.lyr.dat %>% filter(RCP %in% c("rcp8.5",NA), land.cover == "Forestry") %>% 
  mutate(country = ordered(country, levels = c("USA", "Russia", "China","Brazil", "Canada")))


c.rcp26.ls <- lapply(split(c.rcp26, f = c.rcp26$country), split, ~suitability)
c.rcp85.ls <- lapply(split(c.rcp85, f = c.rcp85$country), split, ~suitability)

empty <- ggplot() + theme_minimal()

for (i in 1:10) {
  c.name <- names(c.rcp26.ls[i])
  c.ls.for.26plt <- c.rcp26.ls[i]
  c.ls.for.85plt <- c.rcp85.ls[i]
  
  c.suitability.plt.rcp26.ls <- lapply(c.ls.for.26plt[[1]], suit.plot.func, percent = FALSE)
  c.suitability.plt.rcp85.ls <- lapply(c.ls.for.85plt[[1]], suit.plot.func, percent = FALSE)
  
  c.suitability.perc.plt.rcp26.ls <- lapply(c.ls.for.26plt[[1]], suit.plot.func, percent = TRUE)
  c.suitability.perc.plt.rcp85.ls <- lapply(c.ls.for.85plt[[1]], suit.plot.func, percent = TRUE)
  
  
  c.suitability.area.plt <- ggarrange(empty, 
                                      ggarrange(
                                        ggarrange(c.suitability.plt.rcp26.ls$Unsuitable, c.suitability.plt.rcp26.ls$Marginal, 
                                      c.suitability.plt.rcp26.ls$Moderately, c.suitability.plt.rcp26.ls$Highly, 
                                      c.suitability.plt.rcp85.ls$Unsuitable, c.suitability.plt.rcp85.ls$Marginal,
                                      c.suitability.plt.rcp85.ls$Moderately, c.suitability.plt.rcp85.ls$Highly,
                                      nrow = 2, ncol = 4, labels = c("A.", "B.", "C.", "D.", "E.", "F.", "G.", "H.")),
                                      empty, ncol = 2, widths = c(1, 0.1)), 
                                      heights = c(0.1, 1), nrow = 2)
  
  c.suitability.perc.plt <- ggarrange(empty, 
                                      ggarrange(
                                        ggarrange(c.suitability.perc.plt.rcp26.ls$Unsuitable, c.suitability.perc.plt.rcp26.ls$Marginal, 
                                      c.suitability.perc.plt.rcp26.ls$Moderately, c.suitability.perc.plt.rcp26.ls$Highly, 
                                      c.suitability.perc.plt.rcp85.ls$Unsuitable, c.suitability.perc.plt.rcp85.ls$Marginal,
                                      c.suitability.perc.plt.rcp85.ls$Moderately, c.suitability.perc.plt.rcp85.ls$Highly,
                                      nrow = 2, ncol = 4, labels = c("A.", "B.", "C.","D.","E.", "F.", "G.", "H.")),
                                      empty, ncol = 2, widths = c(1, 0.1)), 
                                      heights = c(0.1, 1), nrow = 2)
  
  c.suitability.area.plt.f <- c.suitability.area.plt + annotation_custom(text_grob("Unsuitable",face = "bold", size = 12), 
                                                                     xmin = 0.15, xmax = 0.15, ymin = 0.95, ymax = 0.95) +
    annotation_custom(text_grob("Marginal",face = "bold", size = 12), 
                      xmin = 0.4, xmax = 0.4, ymin = 0.95, ymax = 0.95)+
    annotation_custom(text_grob("Moderate",face = "bold", size = 12), 
                      xmin = 0.6, xmax = 0.6, ymin = 0.95, ymax = 0.95)+
    annotation_custom(text_grob("High",face = "bold", size = 12), 
                      xmin = 0.85, xmax = 0.85, ymin = 0.95, ymax = 0.95) +
    annotation_custom(text_grob("RCP 2.6",face = "bold", size = 12, rot = 270), 
                      xmin = 0.95, xmax = 0.95, ymin = 0.75, ymax = 0.75)+
    annotation_custom(text_grob("RCP 8.5",face = "bold", size = 12, rot = 270), 
                      xmin = 0.95, xmax = 0.95, ymin = 0.35, ymax = 0.35)
  
  c.suitability.perc.plt.f <- c.suitability.perc.plt + annotation_custom(text_grob("Unsuitable",face = "bold", size = 12), 
                                                                     xmin = 0.15, xmax = 0.15, ymin = 0.95, ymax = 0.95) +
    annotation_custom(text_grob("Marginal",face = "bold", size = 12), 
                      xmin = 0.4, xmax = 0.4, ymin = 0.95, ymax = 0.95)+
    annotation_custom(text_grob("Moderate",face = "bold", size = 12), 
                      xmin = 0.6, xmax = 0.6, ymin = 0.95, ymax = 0.95)+
    annotation_custom(text_grob("High",face = "bold", size = 12), 
                      xmin = 0.85, xmax = 0.85, ymin = 0.95, ymax = 0.95) +
    annotation_custom(text_grob("RCP 2.6",face = "bold", size = 12, rot = 270), 
                      xmin = 0.95, xmax = 0.95, ymin = 0.75, ymax = 0.75)+
    annotation_custom(text_grob("RCP 8.5",face = "bold", size = 12, rot = 270), 
                      xmin = 0.95, xmax = 0.95, ymin = 0.35, ymax = 0.35)
  
  ggsave(path = "Outputs/Figures/Countries", c.suitability.area.plt.f, filename = paste0(c.name, ".Suitable.area.png"),  bg = "white",
         device = "png", width = 30, height = 20, units = "cm")
  ggsave(path = "Outputs/Figures/Countries", c.suitability.perc.plt.f, filename = paste0(c.name, ".Suitable.perc.png"),  bg = "white",
         device = "png", width = 30, height = 20, units = "cm")
}


## Alternate country level plot
ch.rcp26 <- c.rcp26 %>% select(-X.1, -X) %>% pivot_wider(names_from = "suitability", values_from = "area.ha") %>%
  mutate(cult.area.ha = Marginal + Moderately + Highly,
         modhigh.area.ha = Moderately + Highly) %>%
  select(-c(Unsuitable, Marginal, Moderately, Highly)) %>%
  mutate(hist.cult.area.ha = ifelse(time == "Current", cult.area.ha, NA),
         hist.modhigh.area.ha = ifelse(time == "Current", modhigh.area.ha, NA)) %>%
  group_by(country) %>%
  fill(hist.modhigh.area.ha, .direction = "updown") %>%
  fill(hist.cult.area.ha, .direction = "updown") %>%
  filter(!(is.na(RCP))) %>%
  mutate(ch.cult.area.ha = cult.area.ha - hist.cult.area.ha,
         ch.modhigh.area.ha = modhigh.area.ha - hist.modhigh.area.ha,
         ch.cult.area.perc = ch.cult.area.ha/hist.cult.area.ha *100,
         ch.modhigh.area.perc = ch.modhigh.area.ha/hist.modhigh.area.ha *100) %>%
  filter(country %in% c("USA", "Russia", "China", "Brazil", "Canada"))

ch.rcp85 <- c.rcp85 %>% select(-X.1, -X) %>% pivot_wider(names_from = "suitability", values_from = "area.ha") %>%
  mutate(cult.area.ha = Marginal + Moderately + Highly,
         modhigh.area.ha = Moderately + Highly) %>%
  select(-c(Unsuitable, Marginal, Moderately, Highly)) %>%
  mutate(hist.cult.area.ha = ifelse(time == "Current", cult.area.ha, NA),
         hist.modhigh.area.ha = ifelse(time == "Current", modhigh.area.ha, NA)) %>%
  group_by(country) %>%
  fill(hist.modhigh.area.ha, .direction = "updown") %>%
  fill(hist.cult.area.ha, .direction = "updown") %>%
  filter(!(is.na(RCP))) %>%
  mutate(ch.cult.area.ha = cult.area.ha - hist.cult.area.ha,
         ch.modhigh.area.ha = modhigh.area.ha - hist.modhigh.area.ha,
         ch.cult.area.perc = ch.cult.area.ha/hist.cult.area.ha *100,
         ch.modhigh.area.perc = ch.modhigh.area.ha/hist.modhigh.area.ha *100) %>%
  filter(country %in% c("USA", "Russia", "China", "Brazil", "Canada"))

## cult area
c.rcp26.cult.area.plt <- ggplot(filter(ch.rcp26, time != "2010-2039"), 
                             aes(ch.cult.area.ha/1000000, country, fill = time)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(aes(group = time), position = position_dodge(width = 1), size = 1) +
  xlab("Cultivable area change (Mha)") +
  ylab("Country") +
  scale_fill_manual(values = c("#5ab4ac", "#d8b365"), "Time") +
  theme_bw(base_size = 12)


c.rcp85.cult.area.plt <- ggplot(filter(ch.rcp85, time != "2010-2039"), 
                             aes(ch.cult.area.ha/1000000, country, fill = time)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(aes(group = time), position = position_dodge(width = 1), size = 1) +
  xlab("Cultivable area change (Mha)") +
  ylab("Country") +
  scale_fill_manual(values = c("#5ab4ac", "#d8b365"), "Time") +
  theme_bw(base_size = 12)

## cult perc
c.rcp26.cult.perc.plt <- ggplot(filter(ch.rcp26, time != "2010-2039"), 
                                aes(ch.cult.area.perc, country, fill = time)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(aes(group = time), position = position_dodge(width = 1), size = 1) +
  xlab("Cultivable area change (%)") +
  ylab("Country") +
  scale_fill_manual(values = c("#5ab4ac", "#d8b365"), "Time") +
  theme_bw(base_size = 12)


c.rcp85.cult.perc.plt <- ggplot(filter(ch.rcp85, time != "2010-2039"), 
                                aes(ch.cult.area.perc, country, fill = time)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(aes(group = time), position = position_dodge(width = 1), size = 1) +
  xlab("Cultivable area change (%)") +
  ylab("Country") +
  scale_fill_manual(values = c("#5ab4ac", "#d8b365"), "Time") +
  theme_bw(base_size = 12)

## modhigh area
c.rcp26.modhigh.area.plt <- ggplot(filter(ch.rcp26, time != "2010-2039"), 
                                aes(ch.modhigh.area.ha/1000000, country, fill = time)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(aes(group = time), position = position_dodge(width = 1), size = 1) +
  xlab("Mod/high suitability area in forestry change (Mha)") +
  ylab("Country") +
  scale_fill_manual(values = c("#5ab4ac", "#d8b365"), "Time") +
  theme_bw(base_size = 12)


c.rcp85.modhigh.area.plt <- ggplot(filter(ch.rcp85, time != "2010-2039"), 
                                aes(ch.modhigh.area.ha/1000000, country, fill = time)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(aes(group = time), position = position_dodge(width = 1), size = 1) +
  xlab("Mod/high suitability area in forestry change (Mha)") +
  ylab("Country") +
  scale_fill_manual(values = c("#5ab4ac", "#d8b365"), "Time") +
  theme_bw(base_size = 12)

## modhigh perc
c.rcp26.modhigh.perc.plt <- ggplot(filter(ch.rcp26, time != "2010-2039"), 
                                   aes(ch.modhigh.area.perc, country, fill = time)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(aes(group = time), position = position_dodge(width = 1), size = 1) +
  xlab("Mod/high suitability area in forestry change (%)") +
  ylab("Country") +
  scale_fill_manual(values = c("#5ab4ac", "#d8b365"), "Time") +
  theme_bw(base_size = 12)


c.rcp85.modhigh.perc.plt <- ggplot(filter(ch.rcp85, time != "2010-2039"), 
                                   aes(ch.modhigh.area.perc, country, fill = time)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(aes(group = time), position = position_dodge(width = 1), size = 1) +
  xlab("Mod/high suitability area in forestry change (%)") +
  ylab("Country") +
  scale_fill_manual(values = c("#5ab4ac", "#d8b365"), "Time") +
  theme_bw(base_size = 12)

library(ggpubr)
## cult area
c.cult.area.plt <- ggarrange(c.rcp26.cult.area.plt, c.rcp85.cult.area.plt, labels = c("RCP2.6", "RCP8.5"),
                               nrow = 1, common.legend = TRUE, legend = "bottom")

ggsave(path = "Outputs/Figures/Countries", c.cult.area.plt, filename = "country.cult.area.change.png",  bg = "white",
       device = "png", width = 30, height = 12, units = "cm")

## cult perc
c.cult.perc.plt <- ggarrange(c.rcp26.cult.perc.plt, c.rcp85.cult.perc.plt, labels = c("RCP2.6", "RCP8.5"),
                               nrow = 1, common.legend = TRUE, legend = "bottom")

ggsave(path = "Outputs/Figures/Countries", c.cult.perc.plt, filename = "country.cult.perc.change.png",  bg = "white",
       device = "png", width = 30, height = 12, units = "cm")

## modhigh area
c.modhigh.area.plt <- ggarrange(c.rcp26.modhigh.area.plt, c.rcp85.modhigh.area.plt, labels = c("RCP2.6", "RCP8.5"),
                             nrow = 1, common.legend = TRUE, legend = "bottom")

ggsave(path = "Outputs/Figures/Countries", c.modhigh.area.plt, filename = "country.modhigh.area.change.png",  bg = "white",
       device = "png", width = 30, height = 12, units = "cm")

## modhigh perc
c.modhigh.perc.plt <- ggarrange(c.rcp26.modhigh.perc.plt, c.rcp85.modhigh.perc.plt, labels = c("RCP2.6", "RCP8.5"),
                                nrow = 1, common.legend = TRUE, legend = "bottom")

ggsave(path = "Outputs/Figures/Countries", c.modhigh.perc.plt, filename = "country.modhigh.perc.change.png",  bg = "white",
       device = "png", width = 30, height = 12, units = "cm")

#### Final Figures ####
empty <- ggplot() + theme_minimal()

## country mod/high
c.modhigh.perc.area.plt <- ggarrange(empty,
                                ggarrange(c.rcp26.modhigh.area.plt, c.rcp85.modhigh.area.plt,
                                c.rcp26.modhigh.perc.plt, c.rcp85.modhigh.perc.plt,
                                labels = c("A.", "B.", "C.", "D."),
                                nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom"),
                                nrow =2, heights = c(.05, .95))

c.modhigh.perc.area.plt2 <- c.modhigh.perc.area.plt +  
  annotation_custom(text_grob("RCP2.6",face = "bold", size = 12), 
                                                   xmin = 0.3, xmax = 0.3, ymin = 0.97, ymax = 0.97)+
  annotation_custom(text_grob("RCP8.5",face = "bold", size = 12), 
                    xmin = 0.8, xmax = 0.8, ymin = 0.97, ymax = 0.97)

ggsave(path = "Outputs/Figures/Countries", c.modhigh.perc.area.plt2, 
       filename = "country.modhigh.perc.area.change.png",  bg = "white",
       device = "png", width = 25, height = 20, units = "cm")

## country cult
c.cult.perc.area.plt <- ggarrange(empty,
                                     ggarrange(c.rcp26.cult.area.plt, c.rcp85.cult.area.plt,
                                               c.rcp26.cult.perc.plt, c.rcp85.cult.perc.plt,
                                               labels = c("A.", "B.", "C.", "D."),
                                               nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom"),
                                     nrow =2, heights = c(.05, .95))

c.cult.perc.area.plt2 <- c.cult.perc.area.plt +  
  annotation_custom(text_grob("RCP2.6",face = "bold", size = 12), 
                    xmin = 0.3, xmax = 0.3, ymin = 0.97, ymax = 0.97)+
  annotation_custom(text_grob("RCP8.5",face = "bold", size = 12), 
                    xmin = 0.8, xmax = 0.8, ymin = 0.97, ymax = 0.97)

ggsave(path = "Outputs/Figures/Countries", c.cult.perc.area.plt2, 
       filename = "country.cult.perc.area.change.png",  bg = "white",
       device = "png", width = 25, height = 20, units = "cm")

#### Plot continent suitability area ####

cont.rcp26.simple <- cont.lyr.dat %>% filter(RCP %in% c(NA, "rcp2.6"), land.cover == "Forestry") %>% 
  select(-X) %>% 
  pivot_wider(names_from = "suitability", values_from = "area.ha") %>%
  mutate(cult.area.ha = Marginal + Moderately + Highly,
         modhigh.area.ha = Moderately + Highly) %>%
  select(-c(Unsuitable, Marginal, Moderately, Highly)) %>%
  mutate(hist.cult.area.ha = ifelse(time == "Current", cult.area.ha, NA),
         hist.modhigh.area.ha = ifelse(time == "Current", modhigh.area.ha, NA)) %>%
  group_by(continent) %>%
  fill(hist.modhigh.area.ha, .direction = "updown") %>%
  fill(hist.cult.area.ha, .direction = "updown") %>%
  filter(!(is.na(RCP))) %>%
  mutate(ch.cult.area.ha = cult.area.ha - hist.cult.area.ha,
         ch.modhigh.area.ha = modhigh.area.ha - hist.modhigh.area.ha,
         ch.cult.area.perc = ch.cult.area.ha/hist.cult.area.ha *100,
         ch.modhigh.area.perc = ch.modhigh.area.ha/hist.modhigh.area.ha *100)

cont.rcp85.simple <- cont.lyr.dat %>% filter(RCP %in% c(NA, "rcp8.5"), land.cover == "Forestry") %>% select(-X) %>% 
  pivot_wider(names_from = "suitability", values_from = "area.ha") %>%
  mutate(cult.area.ha = Marginal + Moderately + Highly,
         modhigh.area.ha = Moderately + Highly) %>%
  select(-c(Unsuitable, Marginal, Moderately, Highly)) %>%
  mutate(hist.cult.area.ha = ifelse(time == "Current", cult.area.ha, NA),
         hist.modhigh.area.ha = ifelse(time == "Current", modhigh.area.ha, NA)) %>%
  group_by(continent) %>%
  fill(hist.modhigh.area.ha, .direction = "updown") %>%
  fill(hist.cult.area.ha, .direction = "updown") %>%
  filter(!(is.na(RCP))) %>%
  mutate(ch.cult.area.ha = cult.area.ha - hist.cult.area.ha,
         ch.modhigh.area.ha = modhigh.area.ha - hist.modhigh.area.ha,
         ch.cult.area.perc = ch.cult.area.ha/hist.cult.area.ha *100,
         ch.modhigh.area.perc = ch.modhigh.area.ha/hist.modhigh.area.ha *100)

## mod/high area
cont.rcp26.area.plt <- ggplot(filter(cont.rcp26.simple, time != "2010-2039"), 
                                aes(ch.modhigh.area.ha/1000000, continent, fill = time)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(aes(group = time), position = position_dodge(width = 1), size = 1) +
  xlab("Mod/high suitability area change (Mha)") +
  ylab("Country") +
  scale_fill_manual(values = c("#5ab4ac", "#d8b365"), "Time") +
  theme_bw(base_size = 12)

cont.rcp85.area.plt <- ggplot(filter(cont.rcp85.simple, time != "2010-2039"), 
                                aes(ch.modhigh.area.ha/1000000, continent, fill = time)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(aes(group = time), position = position_dodge(width = 1), size = 1) +
  xlab("Mod/high suitability area change (Mha)") +
  ylab("Country") +
  scale_fill_manual(values = c("#5ab4ac", "#d8b365"), "Time") +
  theme_bw(base_size = 12)

## mod/high perc 
cont.rcp26.perc.plt <- ggplot(filter(cont.rcp26.simple, time != "2010-2039"), 
                              aes(ch.modhigh.area.perc, continent, fill = time)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(aes(group = time), position = position_dodge(width = 1), size = 1) +
  xlab("Mod/high suitability area change (%)") +
  ylab("Country") +
  scale_fill_manual(values = c("#5ab4ac", "#d8b365"), "Time") +
  theme_bw(base_size = 12)

cont.rcp85.perc.plt <- ggplot(filter(cont.rcp85.simple, time != "2010-2039"), 
                              aes(ch.modhigh.area.perc, continent, fill = time)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(aes(group = time), position = position_dodge(width = 1), size = 1) +
  xlab("Mod/high suitability area change (%)") +
  ylab("Country") +
  scale_fill_manual(values = c("#5ab4ac", "#d8b365"), "Time") +
  theme_bw(base_size = 12)

## mod high area
cont.modhigh.area.plt <- ggarrange(cont.rcp26.area.plt, cont.rcp85.area.plt, labels = c("RCP2.6", "RCP8.5"),
                                nrow = 1, common.legend = TRUE, legend = "bottom")

ggsave(path = "Outputs/Figures/Continents", cont.modhigh.area.plt, filename = "continent.modhigh.area.change.png",  bg = "white",
       device = "png", width = 30, height = 12, units = "cm")

## mod high perc
cont.modhigh.perc.plt <- ggarrange(cont.rcp26.perc.plt, cont.rcp85.perc.plt, labels = c("RCP2.6", "RCP8.5"),
                                   nrow = 1, common.legend = TRUE, legend = "bottom")

ggsave(path = "Outputs/Figures/Continents", cont.modhigh.perc.plt, filename = "continent.modhigh.perc.change.png",  bg = "white",
       device = "png", width = 30, height = 12, units = "cm")

## continent mod high
cont.modhigh.perc.area.plt <- ggarrange(empty,
                                  ggarrange(cont.rcp26.area.plt, cont.rcp85.area.plt,
                                            cont.rcp26.perc.plt, cont.rcp85.perc.plt,
                                            labels = c("A.", "B.", "C.", "D."),
                                            nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom"),
                                  nrow =2, heights = c(.05, .95))

cont.modhigh.perc.area.plt2 <- cont.modhigh.perc.area.plt +  
  annotation_custom(text_grob("RCP2.6",face = "bold", size = 12), 
                    xmin = 0.3, xmax = 0.3, ymin = 0.97, ymax = 0.97)+
  annotation_custom(text_grob("RCP8.5",face = "bold", size = 12), 
                    xmin = 0.8, xmax = 0.8, ymin = 0.97, ymax = 0.97)

ggsave(path = "Outputs/Figures/Continents", cont.modhigh.perc.area.plt2, 
       filename = "continent.modhigh.perc.area.change.png",  bg = "white",
       device = "png", width = 25, height = 20, units = "cm")
