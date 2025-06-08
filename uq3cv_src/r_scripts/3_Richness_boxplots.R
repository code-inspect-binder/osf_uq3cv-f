rm(list=ls())

setwd()

library(ggpubr)

table <- readxl::read_xlsx('table1_plots_pooled.xlsx')

colnames(table) <- c('duration', 'intensity', 'dayphase', 'richness', 'no_mins')


days_cols <- c("#F0E442","#E69F00","#D55E00","#661100")
intensity_cols <- c("#B7F1B2","#8AE8AE","#54D8AE",'#00B1AC','#11849A')
dawnphase_cols <- c("#AA4499",'#882255','#332288')
table$intensity <- as.factor(table$intensity)

my_comparisons <- list(c("1","4"), c('1','2'), c('2','3'), c('3','4'),
                       c("1","3"), c('2','4'))
ggplot(data=table,aes(x= as.factor(duration), y= richness)) + 
  geom_hline(yintercept=34, linetype="dashed", color = "black", size=1.5)+
  geom_boxplot(aes(x= as.factor(duration), y= richness), fill = days_cols) +
  ylab('Bird Richness') + xlab('Duration') + ylim(0, 65) +
  stat_compare_means(label = 'p.signif', method = "t.test", label.y = c(61, 57,57,57,5, 0),
                     comparisons = my_comparisons, hide.ns = TRUE, size=8) + theme_bw() + theme(text = element_text(size=30)) 

my_comparisons <- list( c("3", "6"), c("6", "15"), c("15", "30"), c("30", "60"),
                        c("60", "15"), c('15','3'),
                        c('30','3'), c('60','3'), c('6','30'), c('6','60'))
ggplot(data=table, aes(x= reorder(intensity, richness), y= richness)) + 
  geom_hline(yintercept=34, linetype="dashed", color = "black", size=1.5) +
  geom_boxplot(aes(reorder(intensity, richness), y= richness), fill = intensity_cols) +
  ylab('Bird Richness') + xlab('Intensity') + ylim(0, 65) +
  stat_compare_means(label = "p.signif", method = "t.test", comparisons = my_comparisons, size=8,
                     label.y = c(54,54,54,54,59,59,10,0,15,5), hide.ns = TRUE) + theme_bw()+  theme(text = element_text(size=30)) 

my_comparisons <- list( c("dawn", "morning"), c("dawn", "both"), c("morning", "both") )
ggplot(data=table, aes(x= reorder(dayphase, richness), y= richness)) + 
  geom_hline(yintercept=34, linetype="dashed", color = "black", size=1.5)+
  geom_boxplot(fill = dawnphase_cols) +
  ylab('Bird Richness') + xlab('Dayphase') + ylim(0, 65) +
  stat_compare_means(aes(x= reorder(dayphase, richness), y= richness), comparisons = my_comparisons, size=8, 
                     label = "p.signif", hide.ns = TRUE, method = "t.test", label.y = c(55,58,55)) + theme_bw() + theme(text = element_text(size=30)) 






