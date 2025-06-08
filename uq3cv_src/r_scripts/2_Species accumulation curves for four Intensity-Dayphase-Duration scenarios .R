rm(list=ls())

library(dplyr)
library(tidyr)
library(vegan)
library(janitor)
library(ggplot2)
library(iNEXT)
library(data.table)
library(tidyverse)
library(grid)
library(betapart)

setwd()

#following code  gets dataset in order
data <- readxl::read_xlsx('identification_data_cleaned.xlsx')
data <- data[,c(2,3,4,5)]
data$species <- paste(data$Genus,data$Species, sep = '_')
colnames(data)[2] <- 'filename'
data2 <- separate(data, col = filename, remove = FALSE,
                  into= c('plot', 'date', 'pre_post_dawn', 'sample_no'), sep = '_')
data2 <- data2[,c(1,2,4:6,9)]

#remove dupblicate species per file, and species level data
data2.5 <- data2[!data2$species == 'Certhia_sp.',]
data2.6 <- data2.5[!data2.5$species == 'Regulus_sp.',]

data3 <- data2.6 %>% group_by(Plot_no) %>% mutate(whichday = as.integer(factor(date)))

data4 <- data3 %>% group_by(Plot_no, filename) %>% mutate(seq=cur_group_id())

###########create a new community matrix for each combination##############
data4$intensity3 <- 1
data4$intensity6 <- ifelse(data4$sample_no == 1 |  data4$sample_no == 3 |data4$sample_no == 5 |data4$sample_no == 7 |
                             data4$sample_no == 9 |data4$sample_no == 11 | data4$sample_no == 13 |data4$sample_no == 15 |
                             data4$sample_no == 17 |data4$sample_no == 19, 1, 0)
data4$intensity15 <- ifelse(data4$sample_no == 1 |  data4$sample_no == 6 |data4$sample_no == 11 |data4$sample_no == 16, 1, 0)
data4$intensity30 <- ifelse(data4$sample_no == 1 |  data4$sample_no == 11, 1, 0)
data4$intensity60 <- ifelse(data4$sample_no == 1, 1, 0)
data4$dayphase_pre <- ifelse(data4$pre_post_dawn == 'predawn', 1, 0)
data4$dayphase_post <- ifelse(data4$pre_post_dawn == 'postdawn', 1, 0)
data4$dayphase_both <- 1
data4$duration1 <- ifelse(data4$whichday < 2, 1, 0)
data4$duration2 <- ifelse(data4$whichday < 3, 1, 0)
data4$duration3 <- ifelse(data4$whichday < 4, 1, 0)
data4$duration4 <- ifelse(data4$whichday < 5, 1, 0)

#remove 7 plots that are not included in days 2-4 (dropped for final analysis)
data4.5 <- data4[,c(1,3)]
data4.6 <- unique(data4.5)
table(data4.6$Plot_no)

data5 <- data4[!data4$Plot_no == 'CFB015' & !data4$Plot_no == 'CFB028' & !data4$Plot_no == 'CFB031' &
                 !data4$Plot_no == 'CFB066' & !data4$Plot_no == 'CFB101' & !data4$Plot_no == 'CFB111' &
                 !data4$Plot_no == 'CFB129' & !data4$Plot_no == 'CFB133' & !data4$Plot_no == 'CFB180',] 
data4 <- data5

durations <- c(17:20)
intensities <- c(9:13)
dayphases <- c(14:16)
blank <- data.frame()
for (i in 1:length(durations)){
  subset1 <- subset(data4, data4[durations[i]] == 1)
  for (j in 1:length(intensities)){
    subset2 <- subset(subset1, subset1[intensities[j]] == 1)
    for (k in 1:length(dayphases)){
      subset3 <- subset(subset2, subset2[dayphases[k]] == 1)
      #subset is made, should total to 60 combinations of subsets
      data5 <- subset3[,c(6,8)]
      combo_x <- as.data.frame(table(data5$species))
      no_samples <- length(unique(data5$seq))
      toprow <- data.frame(Var1='no_samples', Freq = no_samples)
      combo_x <- rbind(toprow, combo_x)
      combo_x$combination <- paste('duration',durations[i],'_intensity', intensities[j], '_dayphase',dayphases[k])
      blank <- rbind(blank, combo_x)
    }
  }
}
data6 <- blank

##add bird community matrix summary 
bird <- readxl::read_xlsx('bird_occurrence_point_counts.xlsx')
bird <- bird[,-c(1,3,4,56)]
bird1 <- transpose(bird)
plotstouse <- unique(data4$Plot_no)
blank <- data.frame()
for (j in 1:length(plotstouse)){
  subset <- bird[bird$plot_id == plotstouse[j],]
  blank <- rbind(blank, subset)
}
bird1 <- blank
bird2 <- as.data.frame(t(bird1))
colnames(bird2) <- bird2[1,]
bird2 <- bird2[-1,]
bird2 <- mutate_all(bird2, function(x) as.numeric(as.character(x)))
bird2$Freq <- rowSums(bird2[1:17])
bird3 <- bird2
bird3$Var1 <- rownames(bird3)
bird3 <- bird3[,c(18,19)]
bird3$combination <- 'pointcount'
bird3 <- bird3[,c(2,1,3)]
newline <- c('no_samples', 340, 'pointcount')
bird3 <- rbind(bird3,newline)

data6.5 <- rbind(data6,bird3)

data7 <- data6.5 %>% spread(combination, Freq)
data7 <- data7[c(34, 1:33,35:62),]
data7[is.na(data7)] <- 0
data8 <- data7[,-1]


data9 <-data8[,c(30,25,43,6,23, 61)] #highest observed, lowest observed, rank 15, rank 30, rank45, point count
data9 <- mutate_all(data9, function(x) as.numeric(as.character(x)))
data10 <- data9[rowSums(data9[])>0,]
my_list2 <- list()
for(i in 1:ncol(data10)) {my_list2[[i]] <- data10[ , i]}
names(my_list2) <- c('Rank 1','Rank 60','Rank 15','Rank 30','Rank 45','Point Count')    # Rename list elements
out.inc <- iNEXT(my_list2, q=0, datatype="incidence_freq", endpoint = 160, nboot = 50)
days_cols <- c("chartreuse4",'mediumpurple3', "darkorange3","#ddcc77", 'deeppink3', "cadetblue3")
ggiNEXT(out.inc, se = TRUE, type=1) +  theme_bw(base_size = 18) + theme(legend.position="right") + ylab("Bird Richness") +
  scale_color_manual(values = days_cols) + scale_fill_manual(values = days_cols) + 
  ylim(0,40) + xlim(1,100) +   xlab('Number of Minutes') 

