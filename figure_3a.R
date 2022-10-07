#!/usr/bin/env Rscript

library(qqman)
library(plyr)
suppressPackageStartupMessages(require(optparse))

option_list = list(
  make_option(c("-i", "--input"), action="store", default=NA, type='character', help="input filename"),
  make_option(c("-o", "--output"), action="store", default=NA, type='character', help="output filename"),
  make_option(c("-t", "--trait"), action="store", default=NA, type='character', help="trait name")
)
opt = parse_args(OptionParser(option_list=option_list))

d <- read.csv(opt$input)
out1 = paste(opt$output, "pdf", sep="")
pdf(out1, width = 10, height = 4)
manhattan(d, main = opt$trait, chr = "Chromosome", bp = "Position", p = "P.value", snp = "SNP", cex = 0.8, logp = TRUE, cex.axis = 0.6, col = c("#A52648", "#396668", "#F08432"), suggestiveline = 5, genomewideline = 7, chrlabs = c("1A", "1B", "1D","2A", "2B", "2D","3A", "3B", "3D","4A", "4B", "4D","5A", "5B", "5D","6A", "6B", "6D","7A", "7B", "7D")) 
dev.off()

out2 = paste(opt$output, "png", sep="")
png(filename = out2, units = "in", res = 300, width = 10, height = 4)

manhattan(d, main = opt$trait, chr = "Chromosome", bp = "Position", p = "P.value", snp = "SNP", cex = 0.8, logp = TRUE, cex.axis = 0.6 , col = c("#A52648", "#396668", "#F08432"), suggestiveline = 5, genomewideline = 7, chrlabs = c("1A", "1B", "1D","2A", "2B", "2D","3A", "3B", "3D","4A", "4B", "4D","5A", "5B", "5D","6A", "6B", "6D","7A", "7B", "7D")) 
dev.off()