
library(terra)
library(tidyterra)
library(tidyverse)
options(scipen = 999)

source("Functions.r")

## get list of layers
layers <- list.files("Data/CurtisLayers/", full.names = TRUE)[1:14]
## storage
lyr.dat <- data.frame()

## loop through the layers pulling out variables and calculating the 
## expanse of each suitability.
for (i in 1:length(layers)) {
  lyr <- layers[i]
  r.lyr <- rast(lyr)
  cat(lyr, '\n', i, "out of", length(layers), '\n')
  
  if (grepl(pattern = "current", lyr) == TRUE){time <- "Current"}
  if (grepl(pattern = "2010", lyr) == TRUE){time <- "2010-2039"}
  if (grepl(pattern = "2040", lyr) == TRUE){time <- "2040-2069"}
  if (grepl(pattern = "2070", lyr) == TRUE){time <- "2070-2099"}
  if (grepl(pattern = "rcp8p5", lyr) == TRUE){rcp <- "rcp8.5"}
  if (grepl(pattern = "rcp2p6", lyr) == TRUE){rcp <- "rcp2.6"}
  if (grepl(pattern = "current", lyr) == TRUE){rcp <- NA}
  if (grepl(pattern = "no.forestry", lyr) == TRUE){land.cover <- "Non-forestry"} else {
    land.cover <- "Forestry"}
  
  tot <- r.lyr %>% expanse(unit = "ha")
  Unsuitable <- r.lyr %>% ifel(. != 1, NA, .) %>% expanse(unit = "ha")
  Marginal <- r.lyr %>% ifel(. != 2, NA, .) %>% expanse(unit = "ha")
  Moderately <- r.lyr %>% ifel(. != 3, NA, .) %>% expanse(unit = "ha")
  Highly <- r.lyr %>% ifel(. != 4, NA, .) %>% expanse(unit = "ha")
  
  lyr.add <- data.frame(suitability = c("Unsuitable", "Marginal", "Moderately", "Highly"),
             area.ha = c(Unsuitable$area, Marginal$area, Moderately$area, Highly$area),
             total.ha = tot$area, RCP = rcp, time = time, land.cover)
  
  lyr.dat <- rbind(lyr.dat, lyr.add)
  
}

#write.csv(lyr.dat, "Data/SuitableArea/SuitableArea.Summary.csv")
lyr.dat <- read.csv("Data/SuitableArea/SuitableArea.Summary.csv")

## make ordered
lyr.dat <- lyr.dat %>%
  mutate(time = ordered(time, levels = c("Current", "2010-2039", "2040-2069","2070-2099")),
         suitability = ordered(suitability, levels = c("Unsuitable", "Marginal", "Moderately", "Highly")))
#### Plot forestry suitability area ####
rcp26 <- lyr.dat %>% filter(RCP %in% c("rcp2.6",NA), land.cover == "Forestry")
rcp85 <- lyr.dat %>% filter(RCP %in% c("rcp8.5",NA), land.cover == "Forestry")

ggplot(rcp26, aes(time, area.ha/1000000, fill = suitability)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  scale_fill_manual(values = c("#cc4c02", "#74c476","#238b45","#005a32")) +
  coord_cartesian(ylim = c(0, 550000000/1000000)) +
  ylab("Forestry agricultural suitability (millions/ha)") +
  xlab("Time period") +
  theme_minimal() +
  theme(legend.title = element_blank())

ggplot(rcp85, aes(time, area.ha/1000000, fill = suitability)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  scale_fill_manual(values = c("#cc4c02", "#74c476","#238b45","#005a32")) +
  coord_cartesian(ylim = c(0, 550000000/1000000)) +
  ylab("Forestry agricultural suitability (millions/ha)") +
  xlab("Time period") +
  theme_minimal() +
  theme(legend.title = element_blank())
  

#### Plot forestry + non forestry suitability area ####
rcp26 <- lyr.dat %>% filter(RCP %in% c("rcp2.6",NA))
rcp85 <- lyr.dat %>% filter(RCP %in% c("rcp8.5",NA))

rcp26.ls <- split(rcp26, f = rcp26$suitability)
rcp85.ls <- split(rcp85, f = rcp85$suitability)

suitability.plt.rcp26.ls <- lapply(rcp26.ls, suit.plot.func, percent = FALSE)
suitability.plt.rcp85.ls <- lapply(rcp85.ls, suit.plot.func, percent = FALSE)

suitability.perc.plt.rcp26.ls <- lapply(rcp26.ls, suit.plot.func, percent = TRUE)
suitability.perc.plt.rcp85.ls <- lapply(rcp85.ls, suit.plot.func, percent = TRUE)

#### Plot forestry + non forestry  moderately suitable area ####
rcp26.mod <- rcp26 %>% filter(suitability %in% c("Moderately", "Highly")) %>%
  group_by(total.ha, RCP, time, land.cover) %>%
  summarise(area.ha = sum(area.ha)) %>% ungroup() %>%
  mutate(area.perc = area.ha/total.ha *100,
         glob.area = sum(unique(total.ha)),
         glob.perc = area.ha/glob.area *100)

rcp85.mod <- rcp85 %>% filter(suitability %in% c("Moderately", "Highly")) %>%
  group_by(total.ha, RCP, time, land.cover) %>%
  summarise(area.ha = sum(area.ha)) %>% ungroup() %>%
  mutate(area.perc = area.ha/total.ha *100,
         glob.area = sum(unique(total.ha)),
         glob.perc = area.ha/glob.area *100)

rcp26.mod.area.plt <- ggplot(rcp26.mod, aes(time, area.ha, colour = land.cover, group = land.cover)) +
  geom_line(linetype = "dashed") +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#005a32", "#8c2d04")) +
  #scale_x_discrete(labels = c("Historic", "2010-2039", "2040-2069","2070-2099")) +
  ylab("Area (ha)") +
  xlab("Time") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45), legend.position = "none")

rcp26.mod.perc.plt <- ggplot(rcp26.mod, aes(time, area.perc, colour = land.cover, group = land.cover)) +
  geom_line(linetype = "dashed") +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#005a32", "#8c2d04")) +
  #scale_x_discrete(labels = c("Historic", "2010-2039", "2040-2069","2070-2099")) +
  ylab("Percentage") +
  xlab("Time") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45), legend.position = "none")

rcp85.mod.area.plt <- ggplot(rcp85.mod, aes(time, area.ha, colour = land.cover, group = land.cover)) +
  geom_line(linetype = "solid") +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#005a32", "#8c2d04")) +
  #scale_x_discrete(labels = c("Historic", "2010-2039", "2040-2069","2070-2099")) +
  ylab("Area (ha)") +
  xlab("Time") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45), legend.position = "none")

rcp85.mod.perc.plt <- ggplot(rcp85.mod, aes(time, area.perc, colour = land.cover, group = land.cover)) +
  geom_line(linetype = "solid") +
  geom_point(size = 3) +
  scale_colour_manual(values = c("#005a32", "#8c2d04")) +
  #scale_x_discrete(labels = c("Historic", "2010-2039", "2040-2069","2070-2099")) +
  ylab("Percentage") +
  xlab("Time") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45), legend.position = "none")

empty <- ggplot() + theme_minimal()

mod.area.plt <- ggarrange(empty, 
                          ggarrange(
                            ggarrange(rcp26.mod.area.plt,rcp85.mod.area.plt,
                           rcp26.mod.perc.plt, rcp85.mod.perc.plt,
                           nrow = 2, ncol = 2, 
                           labels = c("A.", "B.", "C.", "D.")),
                           empty, ncol = 2, widths = c(1, 0.1)), 
                          heights = c(0.1, 1), nrow = 2)

mod.area.plt2 <- mod.area.plt +  annotation_custom(text_grob("RCP2.6",face = "bold", size = 12), 
                    xmin = 0.3, xmax = 0.3, ymin = 0.95, ymax = 0.95)+
  annotation_custom(text_grob("RCP8.5",face = "bold", size = 12), 
                    xmin = 0.8, xmax = 0.7, ymin = 0.95, ymax = 0.95)

ggsave(path = "Outputs/Figures/Global", mod.area.plt2, filename = "Moderately.suitable.area.perc.png",  bg = "white",
       device = "png", width = 30, height = 20, units = "cm")

#### Plot forestry + non forestry  total cultivable area ####
cult.dat <- lyr.dat %>% mutate(cultivable = if_else(suitability == "Unsuitable", 0, 1)) %>%
                        group_by(land.cover, RCP, time, cultivable, total.ha) %>%
                        summarise(cult.area.ha = sum(area.ha)) %>%
  filter(cultivable == 1)

rcp26.cult <- cult.dat %>% filter(RCP %in% c("rcp2.6",NA))
rcp85.cult <- cult.dat %>% filter(RCP %in% c("rcp8.5",NA))

cult.rcp26.area.plt <- cult.plot.func(rcp26.cult, percent = FALSE)
cult.rcp85.area.plt <- cult.plot.func(rcp85.cult, percent = FALSE)
cult.rcp26.perc.plt <- cult.plot.func(rcp26.cult, percent = TRUE)
cult.rcp85.perc.plt <- cult.plot.func(rcp85.cult, percent = TRUE)

#### Arrangement ####

library(ggpubr)

suitability.area.plt <- ggarrange(suitability.plt.rcp26.ls$Unsuitable, suitability.plt.rcp26.ls$Marginal, 
                                  suitability.plt.rcp26.ls$Moderately, suitability.plt.rcp26.ls$Highly, 
                                  suitability.plt.rcp85.ls$Unsuitable, suitability.plt.rcp85.ls$Marginal,
                                  suitability.plt.rcp85.ls$Moderately, suitability.plt.rcp85.ls$Highly,
                                  nrow = 2, ncol = 4, labels = c("A.", "B.", "C.", "D.", "E.", "F.", "G.", "H."))

suitability.perc.plt <- ggarrange(suitability.perc.plt.rcp26.ls$Unsuitable, suitability.perc.plt.rcp26.ls$Marginal, 
                                  suitability.perc.plt.rcp26.ls$Moderately, suitability.perc.plt.rcp26.ls$Highly, 
                                  suitability.perc.plt.rcp85.ls$Unsuitable, suitability.perc.plt.rcp85.ls$Marginal,
                                  suitability.perc.plt.rcp85.ls$Moderately, suitability.perc.plt.rcp85.ls$Highly,
                                  nrow = 2, ncol = 4, labels = c("A.", "B.", "C.","D.", 
                                                                 "E.", "F.", "G.", "H."))
cult.area.plt <- ggarrange(cult.rcp26.area.plt, cult.rcp85.area.plt, 
                                  cult.rcp26.perc.plt, cult.rcp85.perc.plt, 
                                  nrow = 2, ncol = 2, 
                                  labels = c("A.", "B.", "C.", "D."))

empty <- ggplot() + theme_minimal()
suitability.area.plt2 <- ggarrange(empty, 
                                   ggarrange(suitability.area.plt, 
                                             empty, ncol = 2, widths = c(1, 0.1)), 
                                   heights = c(0.1, 1), nrow = 2)

suitability.perc.plt2 <- ggarrange(empty, 
                                   ggarrange(suitability.perc.plt, 
                                             empty, ncol = 2, widths = c(1, 0.1)), 
                                   heights = c(0.1, 1), nrow = 2)

cult.area.plt2 <- ggarrange(empty, cult.area.plt, 
          nrow = 2, heights = c(0.1, 1))

suitability.area.plt3 <- suitability.area.plt2 + annotation_custom(text_grob("Unsuitable",face = "bold", size = 12), 
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


suitability.perc.plt3 <- suitability.perc.plt2 + annotation_custom(text_grob("Unsuitable",face = "bold", size = 12), 
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


ggsave(path = "Outputs/Figures", suitability.area.plt3, filename = "Suitable.area.png",  bg = "white",
       device = "png", width = 30, height = 20, units = "cm")
ggsave(path = "Outputs/Figures", suitability.perc.plt3, filename = "Suitable.perc.png",  bg = "white",
       device = "png", width = 30, height = 20, units = "cm")
ggsave(path = "Outputs/Figures", cult.area.plt2, filename = "Cultivable.area.perc.png",  bg = "white",
       device = "png", width = 30, height = 20, units = "cm")
