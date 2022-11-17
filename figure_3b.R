library(ggplot2)
library(reshape2)
library(ggsci)
library(cowplot)


#####plot and stats for fig. 3b
###Yield temperature
dat <- read.csv('source_data/fig_3b_yield_source_data.csv', header = T)
colnames(dat) <- c("A+A+G", "A+C+C", "T+C+C", "experiment")
melt_dat <- melt(dat)
melt_dat <- melt_dat[!is.na(melt_dat$value),]

mean(melt_dat[melt_dat$experiment == "Heat Stress" & melt_dat$variable=="A+A+G",]$value)
sd(melt_dat[melt_dat$experiment == "Heat Stress" & melt_dat$variable=="A+A+G",]$value)
mean(melt_dat[melt_dat$experiment == "Heat Stress" & melt_dat$variable=="A+C+C",]$value)
sd(melt_dat[melt_dat$experiment == "Heat Stress" & melt_dat$variable=="A+C+C",]$value)
mean(melt_dat[melt_dat$experiment == "Heat Stress" & melt_dat$variable=="T+C+C",]$value)
sd(melt_dat[melt_dat$experiment == "Heat Stress" & melt_dat$variable=="T+C+C",]$value)

res.aov <- aov(value ~ variable, data = melt_dat[melt_dat$experiment == "Heat Stress",])
summary(res.aov)
TukeyHSD(res.aov)

mean(melt_dat[melt_dat$experiment == "Yield Potential" & melt_dat$variable=="A+A+G",]$value)
sd(melt_dat[melt_dat$experiment == "Yield Potential" & melt_dat$variable=="A+A+G",]$value)
mean(melt_dat[melt_dat$experiment == "Yield Potential" & melt_dat$variable=="A+C+C",]$value)
sd(melt_dat[melt_dat$experiment == "Yield Potential" & melt_dat$variable=="A+C+C",]$value)
mean(melt_dat[melt_dat$experiment == "Yield Potential" & melt_dat$variable=="T+C+C",]$value)
sd(melt_dat[melt_dat$experiment == "Yield Potential" & melt_dat$variable=="T+C+C",]$value)

res.aov <- aov(value ~ variable, data = melt_dat[melt_dat$experiment == "Yield Potential",])
summary(res.aov)
TukeyHSD(res.aov)


yield_plot <- ggplot(data = melt_dat, aes(x=variable, y= value, fill=variable)) +
  geom_boxplot() +
  scale_y_continuous(breaks = c(0,100,200,300,400,500,600,700)) +
  theme_bw(base_size = 20) +
  ylab(expression(Yield~(g~m^-2))) +
  xlab(NULL) +
  facet_wrap(~experiment) +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "none") +
  scale_fill_npg() +
  theme(panel.grid.minor = element_blank())



###Canopy temperature
dat <- read.csv('source_data/fig_3b_CT_source_data.csv', header = T)
colnames(dat) <- c("A+A+G", "A+C+C", "T+C+C", "experiment")
melt_dat <- melt(dat)

melt_dat <- melt_dat[!is.na(melt_dat$value),]

mean(melt_dat[melt_dat$experiment == "Heat Stress" & melt_dat$variable=="A+A+G",]$value)
sd(melt_dat[melt_dat$experiment == "Heat Stress" & melt_dat$variable=="A+A+G",]$value)
mean(melt_dat[melt_dat$experiment == "Heat Stress" & melt_dat$variable=="A+C+C",]$value)
sd(melt_dat[melt_dat$experiment == "Heat Stress" & melt_dat$variable=="A+C+C",]$value)
mean(melt_dat[melt_dat$experiment == "Heat Stress" & melt_dat$variable=="T+C+C",]$value)
sd(melt_dat[melt_dat$experiment == "Heat Stress" & melt_dat$variable=="T+C+C",]$value)

res.aov <- aov(value ~ variable, data = melt_dat[melt_dat$experiment == "Heat Stress",])
summary(res.aov)
TukeyHSD(res.aov)

mean(melt_dat[melt_dat$experiment == "Yield Potential" & melt_dat$variable=="A+A+G",]$value)
sd(melt_dat[melt_dat$experiment == "Yield Potential" & melt_dat$variable=="A+A+G",]$value)
mean(melt_dat[melt_dat$experiment == "Yield Potential" & melt_dat$variable=="A+C+C",]$value)
sd(melt_dat[melt_dat$experiment == "Yield Potential" & melt_dat$variable=="A+C+C",]$value)
mean(melt_dat[melt_dat$experiment == "Yield Potential" & melt_dat$variable=="T+C+C",]$value)
sd(melt_dat[melt_dat$experiment == "Yield Potential" & melt_dat$variable=="T+C+C",]$value)

res.aov <- aov(value ~ variable, data = melt_dat[melt_dat$experiment == "Yield Potential",])
summary(res.aov)
TukeyHSD(res.aov)



CT_plot <- ggplot(data = melt_dat, aes(x=variable, y= value, fill=variable)) +
  geom_boxplot() +
  scale_y_continuous(breaks = c(25,27.5,30,32.5,35,37.5,40)) +
  coord_cartesian(ylim = c(25,35)) +
  theme_bw(base_size = 20) +
  ylab(expression(paste('Canopy Temperature (',~degree,'C)',sep=''))) +
  xlab(NULL) +
  facet_wrap(~experiment) +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "none") +
  scale_fill_npg() +
  theme(panel.grid.minor = element_blank())

plot <- cowplot::plot_grid(yield_plot, CT_plot)
plot <- add_sub(plot, "Minor allele combinations at MTAs (6D+1B+2B)", hjust = 0.4, size = 22)


ggsave('fig_3b.pdf', plot, height = 7, width = 20)

