library(tidyterra)
library(tidyverse)
library(ggpubr)
options(scipen = 999)

source("Functions.r")

c.lyr.dat <- read.csv("Data/CountryArea/Timber.top5.suitability.csv")
cont.lyr.dat <- read.csv("Data/ContinentArea/ContinentSuitability.csv")


## make ordered
c.lyr.dat <- c.lyr.dat %>%
  mutate(time = ordered(time, levels = c("Current", "2010-2039", "2040-2069","2070-2099"))) %>%
  filter(land.cover == "Forestry")

#### Plot country suitability area ####
c.rcp26 <- c.lyr.dat %>% filter(RCP %in% c("rcp2.6",NA), land.cover == "Forestry") %>% 
  mutate(country = ordered(country, levels = c("Canada", "Brazil","China", "Russia","USA")))
c.rcp85 <- c.lyr.dat %>% filter(RCP %in% c("rcp8.5",NA), land.cover == "Forestry") %>% 
  mutate(country = ordered(country, levels = c("Canada", "Brazil","China", "Russia","USA")))


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

ch.rcp26 %>% filter(time == "2070-2099") %>% ungroup() %>% tally(ch.modhigh.area.ha)
ch.rcp85 %>% filter(time == "2070-2099") %>% ungroup() %>% tally(ch.modhigh.area.ha)

ch.rcp85 %>% filter(time == "2070-2099") %>% ungroup() %>% tally(total.ha)
ch.rcp26 %>% filter(time == "2070-2099") %>% ungroup() %>% tally(total.ha)

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
  xlab("Change in productive area (M ha)") +
  ylab("Country") +
  scale_fill_manual(values = c("#21918c", "#440154"), "Time Period") +
  theme_bw(base_size = 12) +
  theme(legend.title = element_text(face = "bold"))


c.rcp85.modhigh.area.plt <- ggplot(filter(ch.rcp85, time != "2010-2039"), 
                                   aes(ch.modhigh.area.ha/1000000, country, fill = time)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(aes(group = time), position = position_dodge(width = 1), size = 1) +
  xlab("Change in productive area (M ha)") +
  ylab("Country") +
  scale_fill_manual(values = c("#21918c", "#440154"), "Time Period") +
  theme_bw(base_size = 12) +
  theme(legend.title = element_text(face = "bold"))

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
  xlab("Change in productive area (%)") +
  ylab("Country") +
  scale_fill_manual(values = c("#21918c", "#440154"), "Time Period") +
  theme_bw(base_size = 12) +
  theme(legend.title = element_text(face = "bold"))


c.rcp85.modhigh.perc.plt <- ggplot(filter(ch.rcp85, time != "2010-2039"), 
                                   aes(ch.modhigh.area.perc, country, fill = time)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(aes(group = time), position = position_dodge(width = 1), size = 1) +
  xlab("Change in productive area (%)") +
  ylab("Country") +
  scale_fill_manual(values = c("#21918c", "#440154"), "Time Period") +
  theme_bw(base_size = 12) +
  theme(legend.title = element_text(face = "bold"))

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
                                               labels = c("a", "b", "c", "d"),
                                               nrow = 2, ncol = 2, common.legend = TRUE, legend = "bottom"),
                                     nrow =2, heights = c(.05, .95))

c.modhigh.perc.area.plt2 <- c.modhigh.perc.area.plt +  
  annotation_custom(text_grob("RCP2.6",face = "bold", size = 12), 
                    xmin = 0.3, xmax = 0.3, ymin = 0.97, ymax = 0.97)+
  annotation_custom(text_grob("RCP8.5",face = "bold", size = 12), 
                    xmin = 0.8, xmax = 0.8, ymin = 0.97, ymax = 0.97)

ggsave(path = "Outputs/Figures/Countries", c.modhigh.perc.area.plt2, 
       filename = "country.modhigh.perc.area.change.png",  bg = "white",
       device = "png", width = 20, height = 15, units = "cm")

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
  xlab("Change in productive area (M ha)") +
  ylab("Continent") +
  scale_fill_manual(values = c("#21918c", "#440154"), "Time Period") +
  theme_bw(base_size = 12) +
  theme(legend.title = element_text(face = "bold"))

cont.rcp85.area.plt <- ggplot(filter(cont.rcp85.simple, time != "2010-2039"), 
                              aes(ch.modhigh.area.ha/1000000, continent, fill = time)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(aes(group = time), position = position_dodge(width = 1), size = 1) +
  xlab("Change in productive area (M ha)") +
  ylab("Continent") +
  scale_fill_manual(values = c("#21918c", "#440154"), "Time Period") +
  theme_bw(base_size = 12) +
  theme(legend.title = element_text(face = "bold"))


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
  xlab("Change in productive area (%)") +
  ylab("Continent") +
  scale_fill_manual(values = c("#21918c", "#440154"), "Time Period") +
  theme_bw(base_size = 12) +
  theme(legend.title = element_text(face = "bold"))


cont.rcp85.perc.plt <- ggplot(filter(cont.rcp85.simple, time != "2010-2039"), 
                              aes(ch.modhigh.area.perc, continent, fill = time)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 1.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 4.5, ymax = 5.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 6.5, ymax = 7.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 8.5, ymax = 9.5, colour = NA, fill = "grey75", alpha = .01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_col(aes(group = time), position = position_dodge(width = 1), size = 1) +
  xlab("Change in productive area (%)") +
  ylab("Continent") +
  scale_fill_manual(values = c("#21918c", "#440154"), "Time Period") +
  theme_bw(base_size = 12) +
  theme(legend.title = element_text(face = "bold"))


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


