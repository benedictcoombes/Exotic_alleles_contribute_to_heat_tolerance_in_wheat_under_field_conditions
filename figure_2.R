library(ggplot2)
library(ggh4x)

dat <- read.csv('source_data/fig_2_source_data.csv', header = T)

###stat tests for heat stress conditions
cor.test(dat[dat$Classification == "Exotic-derived" & dat$Condition == "Heat Stress" & dat$Stage == "Grain Filling Stage" & dat$Trait == "NDVI",]$Trait_value, dat[dat$Classification == "Exotic-derived" & dat$Condition == "Heat Stress" & dat$Stage == "Grain Filling Stage" & dat$Trait == "NDVI",]$Yield)
cor.test(dat[dat$Classification == "Elite" & dat$Condition == "Heat Stress" & dat$Stage == "Grain Filling Stage" & dat$Trait == "NDVI",]$Trait_value, dat[dat$Classification == "Elite" & dat$Condition == "Heat Stress" & dat$Stage == "Grain Filling Stage" & dat$Trait == "NDVI",]$Yield)

cor.test(dat[dat$Classification == "Exotic-derived" & dat$Condition == "Heat Stress" & dat$Stage == "Grain Filling Stage" & dat$Trait == "Canopy Temperature",]$Trait_value, dat[dat$Classification == "Exotic-derived" & dat$Condition == "Heat Stress" & dat$Stage == "Grain Filling Stage" & dat$Trait == "Canopy Temperature",]$Yield)
cor.test(dat[dat$Classification == "Elite" & dat$Condition == "Heat Stress" & dat$Stage == "Grain Filling Stage" & dat$Trait == "Canopy Temperature",]$Trait_value, dat[dat$Classification == "Elite" & dat$Condition == "Heat Stress" & dat$Stage == "Grain Filling Stage" & dat$Trait == "Canopy Temperature",]$Yield)

cor.test(dat[dat$Classification == "Exotic-derived" & dat$Condition == "Heat Stress" & dat$Stage == "Vegetative Stage" & dat$Trait == "NDVI",]$Trait_value, dat[dat$Classification == "Exotic-derived" & dat$Condition == "Heat Stress" & dat$Stage == "Vegetative Stage" & dat$Trait == "NDVI",]$Yield)
cor.test(dat[dat$Classification == "Elite" & dat$Condition == "Heat Stress" & dat$Stage == "Vegetative Stage" & dat$Trait == "NDVI",]$Trait_value, dat[dat$Classification == "Elite" & dat$Condition == "Heat Stress" & dat$Stage == "Vegetative Stage" & dat$Trait == "NDVI",]$Yield)

cor.test(dat[dat$Classification == "Exotic-derived" & dat$Condition == "Heat Stress" & dat$Stage == "Vegetative Stage" & dat$Trait == "Canopy Temperature",]$Trait_value, dat[dat$Classification == "Exotic-derived" & dat$Condition == "Heat Stress" & dat$Stage == "Vegetative Stage" & dat$Trait == "Canopy Temperature",]$Yield)
cor.test(dat[dat$Classification == "Elite" & dat$Condition == "Heat Stress" & dat$Stage == "Vegetative Stage" & dat$Trait == "Canopy Temperature",]$Trait_value, dat[dat$Classification == "Elite" & dat$Condition == "Heat Stress" & dat$Stage == "Vegetative Stage" & dat$Trait == "Canopy Temperature",]$Yield)


###stat tests for yield potential conditions
cor.test(dat[dat$Classification == "Exotic-derived" & dat$Condition == "Yield Potential" & dat$Stage == "Grain Filling Stage" & dat$Trait == "NDVI",]$Trait_value, dat[dat$Classification == "Exotic-derived" & dat$Condition == "Yield Potential" & dat$Stage == "Grain Filling Stage" & dat$Trait == "NDVI",]$Yield)
cor.test(dat[dat$Classification == "Elite" & dat$Condition == "Yield Potential" & dat$Stage == "Grain Filling Stage" & dat$Trait == "NDVI",]$Trait_value, dat[dat$Classification == "Elite" & dat$Condition == "Yield Potential" & dat$Stage == "Grain Filling Stage" & dat$Trait == "NDVI",]$Yield)

cor.test(dat[dat$Classification == "Exotic-derived" & dat$Condition == "Yield Potential" & dat$Stage == "Grain Filling Stage" & dat$Trait == "Canopy Temperature",]$Trait_value, dat[dat$Classification == "Exotic-derived" & dat$Condition == "Yield Potential" & dat$Stage == "Grain Filling Stage" & dat$Trait == "Canopy Temperature",]$Yield)
cor.test(dat[dat$Classification == "Elite" & dat$Condition == "Yield Potential" & dat$Stage == "Grain Filling Stage" & dat$Trait == "Canopy Temperature",]$Trait_value, dat[dat$Classification == "Elite" & dat$Condition == "Yield Potential" & dat$Stage == "Grain Filling Stage" & dat$Trait == "Canopy Temperature",]$Yield)

cor.test(dat[dat$Classification == "Exotic-derived" & dat$Condition == "Yield Potential" & dat$Stage == "Vegetative Stage" & dat$Trait == "NDVI",]$Trait_value, dat[dat$Classification == "Exotic-derived" & dat$Condition == "Yield Potential" & dat$Stage == "Vegetative Stage" & dat$Trait == "NDVI",]$Yield)
cor.test(dat[dat$Classification == "Elite" & dat$Condition == "Yield Potential" & dat$Stage == "Vegetative Stage" & dat$Trait == "NDVI",]$Trait_value, dat[dat$Classification == "Elite" & dat$Condition == "Yield Potential" & dat$Stage == "Vegetative Stage" & dat$Trait == "NDVI",]$Yield)

cor.test(dat[dat$Classification == "Exotic-derived" & dat$Condition == "Yield Potential" & dat$Stage == "Vegetative Stage" & dat$Trait == "Canopy Temperature",]$Trait_value, dat[dat$Classification == "Exotic-derived" & dat$Condition == "Yield Potential" & dat$Stage == "Vegetative Stage" & dat$Trait == "Canopy Temperature",]$Yield)
cor.test(dat[dat$Classification == "Elite" & dat$Condition == "Yield Potential" & dat$Stage == "Vegetative Stage" & dat$Trait == "Canopy Temperature",]$Trait_value, dat[dat$Classification == "Elite" & dat$Condition == "Yield Potential" & dat$Stage == "Vegetative Stage" & dat$Trait == "Canopy Temperature",]$Yield)



###plot figure 2
dat$Trait <- relevel(dat$Trait, "NDVI")
levels(dat$Trait) <- c("NDVI", expression(paste('Canopy Temperature (',~degree,'C)',sep='')))
levels(dat$Stage) <- c(expression(Grain~Filling~Stage), expression(Vegetative~Stage))

plot <- ggplot(data = dat, aes(x=Trait_value, y=Yield, colour = Classification,shape=Condition)) +
  geom_point(size = 1.75) +
  geom_smooth(data=subset(dat, Condition %in% c("Heat Stress")), method=lm,se=FALSE) +
  theme_bw() +
  scale_colour_manual(values = c("#4DBBD5FF", "#F39B7FFF")) +
  facet_nested(Stage~Trait, scales = "free", switch = "x", labeller = "label_parsed") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 11)) +
  xlab(NULL) +
  ylab(bquote(Yield~(g~m^-2))) +
  scale_x_continuous(breaks = scales::pretty_breaks(6))

ggsave('/Users/coombes/Documents/heat_paper_new_figures/figure_2.pdf', plot = plot, width = 10, height = 10)
