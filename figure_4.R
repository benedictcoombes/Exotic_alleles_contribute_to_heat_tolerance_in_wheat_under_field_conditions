library(pafr)
library(ggplot2)
library(matrixStats)
library(outliers)
library(janitor)

medianWithoutNA<-function(x) {
  median(x[which(!is.na(x))])
}

dat <- read.table('source_data/fig_4a_read_count_data.tsv', header = T)

seq_depths <- read.table("source_data/fig_4a_seq_depths.txt")

seq_depths_T <- t(seq_depths) %>%
  row_to_names(row_number = 1)
as.integer(seq_depths_T)

dat$position <- dat$position/1000000
df <- data.frame(dat)
df <- df[df$chr!="chrUn",]

vec <- as.integer(as.vector(seq_depths_T))
new_df <- data.frame(df$chr, df$position, sweep(df[,3:ncol(df)], 2, as.integer(seq_depths_T), '/'))


Median <- rowMedians(as.matrix(new_df[,2:ncol(new_df)]))


cov_dev_df <- data.frame(df$chr, df$position)
outlier_results_df <- data.frame(df$chr, df$position)

for (i in colnames(new_df[,3:ncol(new_df)])) {
  line <- new_df[,i]
  
  norm_values <- data.frame(line/Median / medianWithoutNA(line/Median))
  norm_values[is.na(norm_values$line.Median.medianWithoutNA.line.Median.),] <- 1
  cov_dev_df <- cbind(cov_dev_df, norm_values)
  outliers_logical <- outliers::scores(as.numeric(norm_values$line.Median.medianWithoutNA.line.Median.), type = "mad", prob=0.9999)
  outlier_results_df <- cbind(outlier_results_df, outliers_logical)
  
}

colnames(cov_dev_df) <- colnames(df)
colnames(outlier_results_df) <- paste(colnames(df),"_outlier",sep="")

merged_df <- cbind(cov_dev_df, outlier_results_df[,3:ncol(outlier_results_df)])


part_a_plots <- c()

for (i in c("HiBAP_57", "HiBAP_29", "HiBAP_20", "HiBAP_65", "HiBAP_92", "HiBAP_103")) {
  print(i)
  dat <- read.table('source_data/fig_4a_tauschii_snp_data.tsv', header = T)
  dat$position <- dat$position / 1000000
  df <- data.frame(dat)
  df <- df[df$chr=="chr6D",]
  
  df <- df[df$position <= 49,]
  Median <- rowMedians(as.matrix(df[,2:ncol(df)]))
  Mean <- rowMeans(as.matrix(df[,2:ncol(df)]))
  dev_df <- data.frame(df$chr, df$position,df[,i], Median, Mean)
  dev_df$dev <- df[,i] / Mean
  dev_df[dev_df$dev < 1.45,]$dev <- 0
  dev_df
  colnames(dev_df) <- c("chr", "position", "SNPs", "median", "mean", "dev")
  
  sub_df <- data.frame(merged_df[merged_df$chr=="chr6D" & merged_df$position <= 49,]$chr, merged_df[merged_df$chr=="chr6D" & merged_df$position <= 49,]$position, merged_df[merged_df$chr=="chr6D" & merged_df$position <= 49,][,i], merged_df[merged_df$chr=="chr6D" & merged_df$position <= 49,][,paste(i,"_outlier",sep="")])
  colnames(sub_df) <- c("chr", "position", "cov", "sig")
  
  p1 <- ggplot(dat=sub_df) +
    geom_point(aes(x=position, y=cov / median(merged_df[,i]), colour=sig), size=1) +
    scale_x_continuous(limits = c(0,50), breaks = c(0,5,10,15,20,25,30,35,40,45,50)) +
    coord_cartesian(ylim = c(0,2)) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(vjust = -0.6)) +
    labs(x="Chromosomal Position (Mbp)", y=expression(atop("Mapping coverage" , "deviaiton")), title = i) +
    scale_colour_manual(values = c("black", "#EE0000FF")) +
    theme(plot.margin = unit(c(2,0,0,0), "lines"), panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 5.05, colour = "#008B45FF", size = 0.2) +
    geom_vline(xintercept = 6.85, colour = "#008B45FF", size = 0.2) +
    geom_vline(xintercept = 6.276646, colour = "#631879FF", size = 0.2)
  
  
  p2 <- ggplot(dat=dev_df, aes(x=position, y=dev)) +
    geom_col(fill="grey43", colour="black", position = position_nudge(x = 0.5), size = 0.1, width = 1) +
    labs(x="Chromosomal Position (Mbp)", y=expression(atop(paste(italic("Ae. tauschii"),"-specific"), "SNP ratio"))) +
    theme_bw() +
    scale_x_continuous(limits = c(0,50), breaks = c(0,5,10,15,20,25,30,35,40,45,50)) +
    scale_y_continuous(limits = c(0,30), breaks = c(0,5,10,15,20,25,30), labels = scales::number_format(accuracy = 1)) +
    geom_vline(xintercept = 5.05, colour = "#008B45FF", size = 0.2) +
    geom_vline(xintercept = 6.85, colour = "#008B45FF", size = 0.2) +
    geom_vline(xintercept = 6.276646, colour = "#631879FF", size = 0.2) +
    theme(panel.grid.minor = element_blank())
  
  
  plot <- cowplot::plot_grid(p1, p2, align = "v", ncol = 1, rel_heights = c(0.4, 0.6))
  part_a_plots[[i]] <- plot
  
}

fig_1a <- cowplot::plot_grid(plotlist = part_a_plots, ncol = 2)

###plot dotplot

ali <- read_paf('source_data/fig_4b+c_alignment_data.paf')

prim_alignment <- filter_secondary_alignments(ali)
long_ali <- filter_secondary_alignments(subset(ali, alen > 1.5e4 & mapq > 40))

segment_interval <- data.frame(chrom="chr6D:1-10000000", start=5000000, end=6857019)
assoc_interval <- data.frame(chrom="chr6D:1-10000000", start=6276646, end=6276646)

fig_1b <- dotplot(long_ali, xlab=expression(italic("Aegilops tauschii")), y=expression(italic("Tritcium aestivum")), dashes=FALSE, line_size = 1.5) +
  highlight_target(segment_interval, fill="black", alpha = 0, colour = "#008B45FF") +
  highlight_target(assoc_interval, fill="black", alpha = 0, colour = "#631879FF") +
  theme_bw()



###combine plots and save part a and b
tmp <- cowplot::plot_grid(fig_1b, NULL, ncol = 1, labels = c("b", "c"), label_size = 22)
fig_4 <- cowplot::plot_grid(fig_1a, tmp, ncol = 2, align = "h", axis = "t", labels = c("a"), label_size = 22, rel_widths = c(1,0.539))

ggsave('/Users/coombes/Documents/heat_paper_new_figures/fig4.pdf', fig_4, device = "pdf", height=14, width=16)


###plot part c and save. Annotated and combined with part a and b in powerpoint
synteny_plot <- plot_synteny(long_ali,q_chrom="6D:1-10000000", t_chrom ="chr6D:1-10000000") +
  theme_bw()

ggsave('/Users/coombes/Documents/heat_paper_new_figures/fig4-2.pdf', synteny_plot, device = "pdf", height=7, width=15)

