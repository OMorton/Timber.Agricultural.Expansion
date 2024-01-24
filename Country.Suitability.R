library(terra)
library(tidyterra)
library(tidyverse)
library(rnaturalearth)
options(scipen = 999)

source("Functions.r")

#### Country level ####
## Get country outlines
world <- ne_download(scale = "large", returnclass = 'sf')
layers <- list.files("Data/CurtisLayers/", full.names = TRUE)[1:14]
timber.countries <- c("United States of America", "Russia", "China", "Brazil", "Canada",
                      "Indonesia", "Sweden", "Finland", "Germany", "India")

c.lyr.dat <- data.frame()

for (i in 1:length(timber.countries)) {
  
  country.i <- timber.countries[i]
  country.border <- world %>% filter(NAME == country.i)
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
      mask.c.area <- rast(lyr) %>% crop(., c.extent) %>%mask(., vect(country.border))  
                          
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
#write.csv(c.lyr.dat, "Data/CountryArea/Timber.top10.suitability.csv")
c.lyr.dat <- read.csv("Data/CountryArea/Timber.top10.suitability.csv")


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

write.csv(cont.lyr.dat, "Data/ContinentArea/ContinentSuitability.csv")


## make ordered
c.lyr.dat <- c.lyr.dat %>%
  mutate(time = ordered(time, levels = c("Current", "2010-2039", "2040-2069","2070-2099")),
         grp = paste(country, suitability, sep = "."))

#### Plot country suitability area ####
c.rcp26 <- c.lyr.dat %>% filter(RCP %in% c("rcp2.6",NA))
c.rcp85 <- c.lyr.dat %>% filter(RCP %in% c("rcp8.5",NA))


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
c.rcp26.simple <- c.rcp26 %>%
  filter(suitability != c("Unsuitable")) %>%
  group_by(RCP, time, country) %>%
  mutate(cult.area.ha = sum(area.ha)) %>%
  filter(suitability != c("Marginal")) %>%
  mutate(mod.high.area.ha = sum(area.ha),
         hist.mod.high.area.ha = ifelse(time == "Current", mod.high.area.ha, NA),
         hist.cult.area.ha = ifelse(time == "Current", cult.area.ha, NA)) %>%
  group_by(country) %>%
  fill(hist.mod.high.area.ha, .direction = "updown") %>%
  fill(hist.cult.area.ha, .direction = "updown") %>%
  filter(suitability %in% c("Highly", "Moderately")) %>%
  group_by(total.ha, RCP, time, land.cover, country, hist.cult.area.ha, hist.mod.high.area.ha) %>%
  summarise(area.ha = sum(area.ha)) %>% mutate(RCP = "rcp2.6") %>%
  pivot_wider(names_from = time, values_from = area.ha) %>%
  mutate(ch.2099 = `2070-2099` - Current, ch.2069 = `2040-2069` - Current, ch.2039 = `2010-2039` - Current) %>%
  select(-c(`2070-2099`, Current, `2040-2069`, `2010-2039`)) %>%
  pivot_longer(!c(total.ha, hist.cult.area.ha, hist.mod.high.area.ha, RCP, land.cover, country), 
               names_to = "change", values_to = "area.change.ha") %>%
  group_by(country) %>% mutate(country.area = sum(unique(total.ha)),
                               perc.mod.high = area.change.ha/hist.mod.high.area.ha *100,
                               perc.cult = area.change.ha/hist.cult.area.ha *100)

c.rcp85.simple <- c.rcp85 %>%
  filter(suitability != c("Unsuitable")) %>%
  group_by(RCP, time, country) %>%
  mutate(cult.area.ha = sum(area.ha)) %>%
  filter(suitability != c("Marginal")) %>%
  mutate(mod.high.area.ha = sum(area.ha),
         hist.mod.high.area.ha = ifelse(time == "Current", mod.high.area.ha, NA),
         hist.cult.area.ha = ifelse(time == "Current", cult.area.ha, NA)) %>%
  group_by(country) %>%
  fill(hist.mod.high.area.ha, .direction = "updown") %>%
  fill(hist.cult.area.ha, .direction = "updown") %>%
  filter(suitability %in% c("Highly", "Moderately")) %>%
  group_by(total.ha, RCP, time, land.cover, country, hist.cult.area.ha, hist.mod.high.area.ha) %>%
  summarise(area.ha = sum(area.ha)) %>% mutate(RCP = "rcp8.5") %>%
  pivot_wider(names_from = time, values_from = area.ha) %>%
  mutate(ch.2099 = `2070-2099` - Current, ch.2069 = `2040-2069` - Current, ch.2039 = `2010-2039` - Current) %>%
  select(-c(`2070-2099`, Current, `2040-2069`, `2010-2039`)) %>%
  pivot_longer(!c(total.ha, hist.cult.area.ha, hist.mod.high.area.ha, RCP, land.cover, country), 
               names_to = "change", values_to = "area.change.ha") %>%
  group_by(country) %>% mutate(country.area = sum(unique(total.ha)),
                               perc.mod.high = area.change.ha/hist.mod.high.area.ha *100,
                               perc.cult = area.change.ha/hist.cult.area.ha *100)



c.rcp26.simple.plt <- ggplot(filter(c.rcp26.simple, change != "ch.2039"), aes(area.change.ha, country, colour = land.cover, shape = change)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(group = change), position = position_dodge(width = 1), size = 3) +
  xlab("Area change (ha)") +
  ylab("Country") +
  scale_color_manual(values = c("#005a32", "#8c2d04")) +
  theme_bw(base_size = 12)


c.rcp85.simple.plt <- ggplot(filter(c.rcp85.simple, change != "ch.2039"), aes(area.change.ha, country, colour = land.cover, shape = change)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(group = change), position = position_dodge(width = 1), size = 3) +
  xlab("Area change (ha)") +
  ylab("Country") +
  scale_color_manual(values = c("#005a32", "#8c2d04")) +
  theme_bw(base_size = 12)  

c.simple.area.plt <- ggarrange(c.rcp26.simple.plt, c.rcp85.simple.plt, labels = c("RCP2.6", "RCP8.5"),
                               nrow = 1, common.legend = TRUE, legend = "bottom")

ggsave(path = "Outputs/Figures/Countries", c.simple.area.plt, filename = "Country.area.change.png",  bg = "white",
       device = "png", width = 30, height = 12, units = "cm")


### perc suit
c.rcp26.perc.mod.high.plt <- ggplot(filter(c.rcp26.simple, change != "ch.2039"), aes(perc.mod.high, country, colour = land.cover, shape = change)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(group = change), position = position_dodge(width = 1), size = 3) +
  xlab("% change from historic mod/high suitability") +
  ylab("Country") +
  scale_color_manual(values = c("#005a32", "#8c2d04")) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")


c.rcp85.perc.mod.high.plt <- ggplot(filter(c.rcp85.simple, change != "ch.2039"), aes(perc.mod.high, country, colour = land.cover, shape = change)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(group = change), position = position_dodge(width = 1), size = 3) +
  xlab("% change from historic mod/high suitability") +
  ylab("Country") +
  scale_color_manual(values = c("#005a32", "#8c2d04")) +
  theme_bw(base_size = 12)  +
  theme(legend.position = "bottom")

### perc cult
c.rcp26.perc.cult.plt <- ggplot(filter(c.rcp26.simple, change != "ch.2039"), aes(perc.cult, country, colour = land.cover, shape = change)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(group = change), position = position_dodge(width = 1), size = 3) +
  xlab("% change from historic cultivable area") +
  ylab("Country") +
  scale_color_manual(values = c("#005a32", "#8c2d04")) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")


c.rcp85.perc.cult.plt <- ggplot(filter(c.rcp85.simple, change != "ch.2039"), aes(perc.cult, country, colour = land.cover, shape = change)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(group = change), position = position_dodge(width = 1), size = 3) +
  xlab("% change from historic cultivable area") +
  ylab("Country") +
  scale_color_manual(values = c("#005a32", "#8c2d04")) +
  theme_bw(base_size = 12)  +
  theme(legend.position = "bottom")

c.perc.mod.high.plt <- ggarrange(c.rcp26.perc.mod.high.plt, c.rcp85.perc.mod.high.plt, labels = c("RCP2.6", "RCP8.5"),
                                 nrow = 1, common.legend = TRUE, legend = "bottom")

c.perc.cult.plt <- ggarrange(c.rcp26.perc.cult.plt, c.rcp85.perc.cult.plt, labels = c("RCP2.6", "RCP8.5"),
                             nrow = 1, common.legend = TRUE, legend = "bottom")

ggsave(path = "Outputs/Figures/Countries", c.perc.mod.high.plt, filename = "Country.mod.high.perc.change.png",  bg = "white",
       device = "png", width = 25, height = 12, units = "cm")

ggsave(path = "Outputs/Figures/Countries", c.perc.cult.plt, filename = "Country.mod.cult.change.png",  bg = "white",
       device = "png", width = 25, height = 12, units = "cm")

#### Plot continent suitability area ####

cont.rcp26.simple <- cont.lyr.dat %>% filter(suitability %in% c("Highly", "Moderately"), RCP %in% c("rcp2.6", NA)) %>%
  group_by(total.ha, RCP, time, land.cover, continent) %>%
  summarise(area.ha = sum(area.ha)) %>% mutate(RCP = "rcp2.6") %>%
  pivot_wider(names_from = time, values_from = area.ha) %>%
  mutate(ch.2099 = `2070-2099` - Current, ch.2069 = `2040-2069` - Current, ch.2039 = `2010-2039` - Current) %>%
  select(-c(`2070-2099`, Current, `2040-2069`, `2010-2039`)) %>%
  pivot_longer(!c(total.ha, RCP, land.cover, continent), names_to = "change", values_to = "area.change.ha")

cont.rcp85.simple <- cont.lyr.dat %>% filter(suitability %in% c("Highly", "Moderately"), RCP %in% c("rcp8.5", NA)) %>%
  group_by(total.ha, RCP, time, land.cover, continent) %>%
  summarise(area.ha = sum(area.ha)) %>% mutate(RCP = "rcp8.5") %>%
  pivot_wider(names_from = time, values_from = area.ha) %>%
  mutate(ch.2099 = `2070-2099` - Current, ch.2069 = `2040-2069` - Current, ch.2039 = `2010-2039` - Current) %>%
  select(-c(`2070-2099`, Current, `2040-2069`, `2010-2039`)) %>%
  pivot_longer(!c(total.ha, RCP, land.cover, continent), names_to = "change", values_to = "area.change.ha")

cont.rcp26.simple.plt <- ggplot(filter(cont.rcp26.simple, change != "ch.2039"), aes(area.change.ha, continent, colour = land.cover, shape = change)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(group = change), position = position_dodge(width = 1), size = 3) +
  scale_color_manual(values = c("#005a32", "#8c2d04")) +
  theme_bw()

cont.rcp85.simple.plt <- ggplot(filter(cont.rcp85.simple, change != "ch.2039"), aes(area.change.ha, continent, colour = land.cover, shape = change)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(group = change), position = position_dodge(width = 1), size = 3) +
  scale_color_manual(values = c("#005a32", "#8c2d04")) +
  theme_bw()

ggsave(path = "Outputs/Figures/Continents", cont.rcp26.simple.plt, filename = "Continent.area.change.rcp26.png",  bg = "white",
       device = "png", width = 17, height = 12, units = "cm")
ggsave(path = "Outputs/Figures/Continents", cont.rcp85.simple.plt, filename = "Continent.area.change.rcp85.png",  bg = "white",
       device = "png", width = 17, height = 12, units = "cm")
#### arrangement ####

168596211 + 736417454
1003604298 -905013665
98,590,633

country.border <- world %>% filter(SOVEREIGNT == "Portugal")

c.extent <- terra::ext(country.border)
r.lyr.crop.f <- terra::crop(forest, c.extent)
mask.c.area.f <- mask(r.lyr.crop.f, vect(country.border))  

r.lyr.crop.nf <- terra::crop(nforest, c.extent)
mask.c.area.nf <- mask(r.lyr.crop.nf, vect(country.border))

ggplot() +
  geom_spatraster(data = mask.c.area.f) +
  geom_spatraster(data = mask.c.area.nf) +
  geom_spatvector(data = country.border, fill = NA) +
  scale_fill_continuous(na.value = NA)

expanse(vect(country.border), unit = "km")
expanse(mask.c.area.f, unit = "km")
expanse(mask.c.area.nf, unit = "km")
45659.83 + 42858.85

forest <- rast("Data/CurtisLayers/curtis.forestry.2010.2039.rcp2p6.ag.suitability.classified 1.tif")
nforest <- rast("Data/CurtisLayers/curtis.no.forestry.2010.2039.rcp2p6.ag.suitability.classified.tif")

plot(nforest)

ggplot() +
  geom_spatraster(data = forest) +
  geom_spatraster(data = nforest) +
  scal
