library(tidyterra)
library(tidyverse)
library(ggpubr)
library(terra)
options(scipen = 999)

source("Functions.r")

c.lyr.dat <- read.csv("Data/CountryArea/Timber.top5.suitability.csv")
cont.lyr.dat <- read.csv("Data/ContinentArea/ContinentSuitability.csv")
all.c.dat <- read.csv("Data/CountryArea/all.country.change.area.2070.2099.raw.csv")
all.c.dat.sum <- read.csv("Data/CountryArea/all.country.change.area.2070.2099.sum.csv")
all.c.gain.loss.dat <- read.csv("Data/CountryArea/all.country.gain.loss.raw.csv")
tt.dat <- read.csv("Data/TravelTime/travel.time.sum.csv")
d.dat <- read.csv("Data/Distance/distance.sum.csv")
load("Data/CountryCrop/top.countries.5.top.crops.gains.losses.df.RData")
load("Data/CountryCrop/five.crops.global.productive.land.change.df.RData")

country.forest.area <- all.c.dat.sum %>% 
  group_by(country, rcp) %>% summarise(tot.forestry.area = sum(area)) %>%
 filter(rcp == "rcp2p6") %>% select(-rcp)
top.countries.5.top.crop.df <- top.countries.5.top.crop.df %>%
  mutate(crop = factor(crop, levels = c("Potato", "Soy", "Wheat", "Maize", "Rice")),
         net.area.gain.mha = (area.gained - area.loss)/1000000)
five.crops.global.productive.land.change.df <- five.crops.global.productive.land.change.df %>%
  mutate(crop = factor(crop, levels = c("Potato", "Soy", "Wheat", "Maize", "Rice")))
## make ordered
c.lyr.dat <- c.lyr.dat %>%
  mutate(time = ordered(time, levels = c("Current", "2010-2039", "2040-2069","2070-2099"))) %>%
  filter(land.cover == "Forestry")

#### Horizontal top 4 plots net change free scales ####
Rus.plt <- ggplot(filter(top.countries.5.top.crop.df, country == "Russia"), 
                  aes(net.area.gain.mha, crop, group = scenario, fill = scenario)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #geom_col() +
  geom_col(position = position_dodge()) +
  xlab("Change in productive area (M ha)") +
  ylab("Crop") +
  scale_fill_manual(values = c("#21918c", "#440154"), "RCP") +
  coord_cartesian(xlim = c(0, 45)) +
  annotate("text", label = "Russia", y = 5.3, x = 45*0.9, fontface = "bold", size = 4.5) +
  theme_bw(base_size = 12) +
  theme(legend.title = element_text(face = "bold"),
        strip.background = element_blank(), strip.text = element_text(face ="bold"),
        legend.position = "bottom")

USA.plt <- ggplot(filter(top.countries.5.top.crop.df, country == "United States"), 
                  aes(net.area.gain.mha, crop, group = scenario, fill = scenario)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #geom_col() +
  geom_col(position = position_dodge()) +
  xlab("Change in productive area (M ha)") +
  ylab("Crop") +
  scale_fill_manual(values = c("#21918c", "#440154"), "RCP") +
  coord_cartesian(xlim = c(-0.2, 3)) +
  annotate("text", label = "USA", y = 5.3, x = 3*0.9, fontface = "bold", size = 4.5) +
  theme_bw(base_size = 12) +
  theme(legend.title = element_text(face = "bold"),
        strip.background = element_blank(), strip.text = element_text(face ="bold"),
        legend.position = "bottom")

Can.plt <- ggplot(filter(top.countries.5.top.crop.df, country == "Canada"), 
                  aes(net.area.gain.mha, crop, group = scenario, fill = scenario)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #geom_col() +
  geom_col(position = position_dodge()) +
  xlab("Change in productive area (M ha)") +
  ylab("Crop") +
  scale_fill_manual(values = c("#21918c", "#440154"), "RCP") +
  coord_cartesian(xlim = c(0, 8)) +
  annotate("text", label = "Canada", y = 5.3, x = 8*0.9, fontface = "bold", size = 4.5) +
  theme_bw(base_size = 12) +
  theme(legend.title = element_text(face = "bold"),
        strip.background = element_blank(), strip.text = element_text(face ="bold"),
        legend.position = "bottom")

Chi.plt <- ggplot(filter(top.countries.5.top.crop.df, country == "China"), 
                  aes(net.area.gain.mha, crop, group = scenario, fill = scenario)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #geom_col() +
  geom_col(position = position_dodge()) +
  xlab("Change in productive area (M ha)") +
  ylab("Crop") +
  scale_fill_manual(values = c("#21918c", "#440154"), "RCP") +
  coord_cartesian(xlim = c(0, 4.5)) +
  annotate("text", label = "China", y = 5.3, x = 4.5*0.9, fontface = "bold", size = 4.5) +
  theme_bw(base_size = 12) +
  theme(legend.title = element_text(face = "bold"),
        strip.background = element_blank(), strip.text = element_text(face ="bold"),
        legend.position = "bottom")


top.4.crop.df <- top.countries.5.top.crop.df %>% 
  group_by(time, scenario, crop) %>% summarise(net.area.gain.mha = sum(net.area.gain.mha))
top4.plt <- ggplot(top.4.crop.df, 
                   aes(net.area.gain.mha, crop, group = scenario, fill = scenario)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #geom_col() +
  geom_col(position = position_dodge()) +
  xlab("Change in productive area (M ha)") +
  ylab("Crop") +
  scale_fill_manual(values = c("#21918c", "#440154"), "RCP") +
  coord_cartesian(xlim = c(0, 50)) +
  annotate("text", label = "Top 4", y = 5.3, x = 50*0.97, fontface = "bold", size = 4.5) +
  theme_bw(base_size = 12) +
  theme(legend.title = element_text(face = "bold"),
        strip.background = element_blank(), strip.text = element_text(face ="bold"),
        legend.position = "bottom")

All.plt <- ggplot(five.crops.global.productive.land.change.df, 
                  aes(net.change.mha, crop, group = scenario, fill = scenario)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  #geom_col() +
  geom_col(position = position_dodge()) +
  xlab("Change in productive area (M ha)") +
  ylab("Crop") +
  scale_fill_manual(values = c("#21918c", "#440154"), "RCP") +
  coord_cartesian(xlim = c(0, 50)) +
  annotate("text", label = "Global", y = 5.3, x = 50*0.97, fontface = "bold", size = 4.5) +
  theme_bw(base_size = 12) +
  theme(legend.title = element_text(face = "bold"),
        strip.background = element_blank(), strip.text = element_text(face ="bold"),
        legend.position = "bottom")

top4.arr.plt.freescales <- ggarrange(All.plt,
                                     ggarrange(Rus.plt,USA.plt, Can.plt, Chi.plt, 
                                               nrow = 2, ncol = 2,
                                               labels = c("b", "c", "d", "e", "f"), common.legend = TRUE, legend = "bottom"),
                                     nrow = 2, labels = c("a", ""), legend = "none", heights = c(1,2))

ggsave(path = "Outputs/Figures/CountryCrop", top4.arr.plt.freescales, filename = "top4.plusALL.freescales.png",  bg = "white",
       device = "png", width = 20, height = 16, units = "cm")
#### Country gain loss ####
forestry.area <- rast("Data/CurtisLayers/curtis.forestry.2010.2039.rcp2p6.ag.suitability.classified 3.tif") %>%
  expanse(unit = "ha") ## 1,197,895,027

zabel.lyr <- rast("Data/Zabel/overall_suitability_subset_1to17.bil")
total.area.z <-  expanse(zabel.lyr, unit = "ha") ## 12,724,713,608

glob.gain.loss <- read.csv("Data/CountryArea/global.gain.loss.raw.csv")
all.gain.loss.mtop4 <- read.csv("Data/CountryArea/global.gain.loss.raw.minus.top4.csv")


glob.sum <- glob.gain.loss %>% 
  filter(suitability =="Gain") %>%
  group_by(rcp, time) %>%
  mutate(world.gain = sum(area), world.area = total.area.z$area,
         gain.perc = area/world.gain *100, area.perc = forestry.area$area/world.area *100,
         rel.gain = gain.perc/area.perc) %>%
  filter(land.cover == "forestry", time == "2070-2099")

glob.mtop4.sum <- all.gain.loss.mtop4 %>% 
  filter(suitability =="Gain") %>%
  group_by(rcp, time) %>%
  mutate(world.gain = sum(area), world.area = total.area.z$area,
         gain.perc = area/world.gain *100, area.perc = forestry.area$area/world.area *100,
         rel.gain = gain.perc/area.perc) %>%
  filter(land.cover == "forestry", time == "2070-2099")

test <- all.c.gain.loss.dat %>% select(-c(X, layer, value)) %>% 
  ## sum with these groups so countries (India with multiple GIDs for disputed territories are
  ## incorpoated into India)
  group_by(country, rcp, time, land.cover, suitability) %>%
  summarise(area = sum(area)) %>%
  filter(suitability =="Gain") %>%
  left_join(country.forest.area) %>%
  group_by(rcp, time) %>%
  mutate(world.gain = sum(area), world.area = total.area.z$area,
         gain.perc = area/world.gain *100, area.perc = tot.forestry.area/world.area *100)


test %>% filter(rcp == "rcp8.5", time == "2010-2039") %>% ungroup() %>% tally(gain.perc)
test %>% filter() %>% group_by(land.cover, time, rcp) %>% tally(gain.perc)

test2 <- test %>% filter(land.cover == "forestry")

topprod <- test2 %>% mutate(order = case_when(country == "United States" ~ 1,
                                              country == "Russia" ~ 2,
                                              country == "China" ~ 3,
                                              country == "Brazil" ~ 4,
                                              country == "Canada" ~ 5,
                                              country == "Indonesia" ~ 6,
                                              country == "Sweden" ~ 7,
                                              country == "Finland" ~ 8,
                                              country == "Germany" ~ 9,
                                              country == "India" ~ 10),
                            order2 = case_when(country == "United States" ~ 2,
                                              country == "Russia" ~ 1,
                                              country == "China" ~ 4,
                                              country == "Canada" ~ 3),
                            rel.gain = gain.perc/area.perc) %>%
  filter(!is.na(order2), time == "2070-2099")

top4.ordered.plot <- ggplot(topprod, aes(order2, rel.gain, colour = rcp)) +
  annotate("rect", xmin = -0.5, xmax = 0.5, ymin = -Inf, ymax = Inf, fill = "grey75", alpha= .5) +
  geom_point(aes(group = rcp), position = position_dodge(width = .7), size = 3) +
  geom_point(data = glob.sum, aes(x = 0, y = rel.gain, fill = rcp),
             position = position_dodge(width = .7), size = 5, shape = 21, colour = "black") +
  geom_point(data = glob.mtop4.sum, aes(x = 5, y = rel.gain, fill = rcp),
             position = position_dodge(width = .7), size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_cartesian(xlim = c(-0.5, 5.5), ylim = c(0, 7), expand = FALSE) +
  scale_x_continuous(breaks = c(0:5), 
                     labels = c("Global", "Russia", "United States",  "Canada", "China", "Rest of \n the world")) +
  scale_color_manual(values = c("#21918c", "#440154"), "RCP", 
                     labels = c("RCP2.6", "RCP8.5")) +
  scale_fill_manual(values = c("#21918c", "#440154"), "RCP", 
                     labels = c("RCP2.6", "RCP8.5")) +
  ylab("Productive frontier in  \n forestry : Land surface area ratio") +
  theme_bw(base_size = 11.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = c("bold", rep("plain", 10)),
                                   colour = "black"),
        legend.position = "bottom", axis.title.x = element_blank(),
        legend.title = element_text(face = "bold"))

ggsave(path = "Outputs/Figures/Countries", top10.ordered.plot, 
       filename = "country.gainloss.top4.ratio.png",  bg = "white",
       device = "png", width = 15, height = 10, units = "cm")

#### Travel + distance ####

tt.plt <- ggplot(tt.dat, aes(mean/60, land.cover, colour = rcp, group = rcp)) +
  geom_point(position = position_dodge(width = 0.7), size = 4) +
  geom_errorbarh(aes(xmin = q5/60, xmax = q95/60), position = position_dodge(width = 0.7),
                 height = 0, linewidth = 1) +
  geom_errorbarh(aes(xmin = q25/60, xmax = q75/60), position = position_dodge(width = 0.7),
                 height = 0, linewidth = 2) +
  scale_y_discrete(limits = c("non.forestry", "forestry"), 
                   labels = c("Non forestry", "Forestry")) +
  scale_color_manual(values = c("#21918c", "#440154"), "RCP", 
                     labels = c("RCP2.6", "RCP8.5")) +
  xlab("Travel time to population centre (hours)") +
  ylab("Productive frontier") +
  #scale_x_log10() +
  theme_bw(base_size = 11.5) +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"))
  

d.plt <- ggplot(d.dat, aes(mean/1000, land.cover, colour = rcp, group = rcp)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = q5/1000, xmax = q95/1000), position = position_dodge(width = 0.7),
                 height = 0, linewidth = 1) +
  geom_errorbarh(aes(xmin = q25/1000, xmax = q75/1000), position = position_dodge(width = 0.7),
                 height = 0, linewidth = 2) +
  scale_y_discrete(limits = c("non.forestry", "forestry"), 
                   labels = c("Non forestry", "Forestry")) +
  scale_color_manual(values = c("#21918c", "#440154"), "RCP", 
                     labels = c("RCP2.6", "RCP8.5")) +
  xlab("Distance to current agriculture (km)") +
  ylab("Productive frontier") +
  #scale_x_log10() +
  theme_bw(base_size = 11.5) +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"))

tt.d.plt <- ggarrange(top4.ordered.plot, 
                      ggarrange(tt.plt, d.plt, labels = c("b", "c"), nrow = 1,
                                ncol = 2, common.legend = TRUE, legend = "bottom"),
                      legend = "none", labels = c("a", ""), ncol = 1, nrow = 2, 
                      heights = c(1, 1))

ggsave(path = "Outputs/Figures/Countries", tt.d.plt, 
       filename = "country.gainloss.ratio.tt.d.png",  bg = "white",
       device = "png", width = 20, height = 16, units = "cm")


tt.d.plt <- ggarrange(top4.ordered.plot, tt.plt, d.plt, labels = c("a", "b", "c"), 
                      nrow = 3, ncol = 1, common.legend = TRUE, legend = "bottom")

ggsave(path = "Outputs/Figures/Countries", tt.d.plt, 
       filename = "country.gainloss.ratio.tt.d.tall.png",  bg = "white",
       device = "png", width = 14, height = 22, units = "cm")
