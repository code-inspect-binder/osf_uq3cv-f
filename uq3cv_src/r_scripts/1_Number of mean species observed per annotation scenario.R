rm(list=ls())
library(ggplot2)

setwd()


table <- read.csv('table1_plots_local.csv')
table2 <- table[-c(1,62),]
colnames(table2) <- c('intensity', 'dayphase', 'duration', 'No_minutes', 'Observed.Mean', "observedSE", 'rankobserved')
table2$Observed.Mean <- as.numeric(table2$Observed.Mean)
table2$observedSE <- as.numeric(table2$observedSE)
table2$No_minutes <- as.numeric(table2$No_minutes)

ggplot(table2, aes(No_minutes, Observed.Mean)) + 
  geom_point(size = 3) + theme_bw(base_size = 18) +
  geom_errorbar(aes(ymin=Observed.Mean- observedSE, ymax=Observed.Mean + observedSE), size=0.5, width = 2, alpha=.5) +
  ylab('Number of Mean Species Observed') + xlab("Number of Minutes per Sampling Combination") +
  geom_point(aes(x=20, y=12.96), colour="black", size = 3, shape=1, stroke = 1.5) + 
  geom_errorbar(aes(ymin = 12.365, ymax = 13.868, x = 20), size = 0.5, width=2, color='black')
ggsave("//unfnsh01.public.ads.uni-freiburg.de/unfnsh01/Unfn_ConFoBi/3_projects/B7/P8_Sampling_Methods/figures/draft5/figure2.png", dpi = 300) 



