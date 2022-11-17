library(ggplot2)
library(reshape2)
library(ggsci)


dat <- read.csv('source_data/fig_1c_heat_source_data.csv', header = T)

Elites <- dat[dat$Group == "Elite",]
Exotics <- dat[dat$Group == "Exotic-der",]


for (i in colnames(Elites[,2:ncol(Elites)])) {
  print(i)
  print(shapiro.test(as.numeric(unlist(Elites[i]))))
  print(shapiro.test(as.numeric(unlist(Elites[i]))))
  print(t.test(Elites[i], Exotics[i]))
  print(mean(as.numeric(unlist(Elites[i]))))
  print(sd(as.numeric(unlist(Elites[i]))))
  print(mean(as.numeric(unlist(Exotics[i]))))
  print(sd(as.numeric(unlist(Exotics[i]))))
}


melt_dat <- melt(dat)

melt_dat$variable <- factor(melt_dat$variable,
                            levels=c("YLD", "TGW", "GM2", "BM", "HI", "Height"),
                            labels=c("Yield (g m{}^-2)","TGW","GM2", "BM", "HI", "Height"))

labels <- expression("YLD " ~ (g~m ^{-2}),"TGW (g)","GM2 " ~ (no.~grains ~m^{-2}), "BM " ~ (g~m ^{-2}), "HI", "Height (cm)")

boxplot_heat_stress_plot <- ggplot(data = melt_dat, aes(x=Group, y=value, fill=Group)) +
  geom_boxplot() +
  facet_wrap(~ variable, strip.position="left", ncol = 6, scales = "free", labeller = function(x) {
    list(as.list(labels)[x$variable])}) +
  theme_bw() +
  ylab(NULL) +
  xlab(NULL) +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "none") +
  scale_y_continuous(breaks = scales::pretty_breaks(4)) +
  scale_fill_manual(values = c("#45c2c6", "#f67e7b")) +
  theme(plot.margin = unit(c(1,1,0.5,0), "lines")) +
  labs(title="Heat Stress Experiments") +
  theme(plot.title = element_text(hjust = 0.5))

dat <- read.csv('source_data/fig_1c_yield_potential_source_data.csv', header = T)

Elites <- dat[dat$Group == "Elite",]
Exotics <- dat[dat$Group == "Exotic-der",]



for (i in colnames(Elites[,2:ncol(Elites)])) {
  print(i)
  print(shapiro.test(as.numeric(unlist(Elites[i]))))
  print(shapiro.test(as.numeric(unlist(Elites[i]))))
  print(t.test(Elites[i], Exotics[i]))
  print(mean(as.numeric(unlist(Elites[i]))))
  print(sd(as.numeric(unlist(Elites[i]))))
  print(mean(as.numeric(unlist(Exotics[i]))))
  print(sd(as.numeric(unlist(Exotics[i]))))
}

melt_dat <- melt(dat)

melt_dat$variable <- factor(melt_dat$variable,
                            levels=c("YLD", "TGW", "GM2", "BM", "HI", "Height"),
                            labels=c("Yield (g m{}^-2)","TGW","GM2", "BM", "HI", "Height"))

labels <- expression("YLD " ~ (g~m ^{-2}),"TGW (g)","GM2 " ~ (no.~grains ~m^{-2}), "BM " ~ (g~m ^{-2}), "HI", "Height (cm)")

boxplot_yield_potential_plot <- ggplot(data = melt_dat, aes(x=Group, y=value, fill=Group)) +
  geom_boxplot() +
  facet_wrap(~ variable, strip.position="left", ncol = 6, scales = "free", labeller = function(x) {
    list(as.list(labels)[x$variable])}) +
  theme_bw() +
  ylab(NULL) +
  xlab(NULL) +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "none") +
  scale_y_continuous(breaks = scales::pretty_breaks(4)) +
  scale_fill_manual(values = c("#45c2c6", "#f67e7b")) +
  theme(plot.margin = unit(c(1,1,0.5,0), "lines")) +
  labs(title="Yield Potential Experiments") +
  theme(plot.title = element_text(hjust = 0.5))


###make percentage loss plot
dat <- read.csv('source_data/fig_1b_source_data.csv', header = T)
dat$pos <- dat$Percentage_loss>=0

percentage_change_plot <- ggplot(data=dat, aes(x=Trait, y=Percentage_loss, fill=pos)) +
  geom_col(colour="black") +
  coord_flip() +
  theme_classic() +
  labs(x=NULL, y="% change under heat stress vs yield potential") +
  scale_fill_manual(values = c("#BC3C29FF", "royalblue2")) +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0) +
  geom_text(aes(label=Percentage_loss), colour="black", hjust=1.1, size=3) +
  scale_y_continuous(limits = c(-55, 30))


###
part_c <- cowplot::plot_grid(boxplot_heat_stress_plot, boxplot_yield_potential_plot, ncol = 1)
part_ab <- cowplot::plot_grid(NULL, percentage_change_plot, labels = c("a", "b", label_size = 22))
final_plot <- cowplot::plot_grid(part_ab, part_c, ncol = 1, labels = c("", "c", label_size = 22))


ggsave('figure_1.pdf', plot = final_plot, device = "pdf", width = 14, height = 11)
