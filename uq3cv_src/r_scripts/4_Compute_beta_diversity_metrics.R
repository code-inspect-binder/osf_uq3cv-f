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

#following code just gets dataset in order
data <- readxl::read_xlsx('identification_data_cleaned.xlsx')
data <- data[,c(2,3,4,5)]
data$species <- paste(data$Genus,data$Species, sep = '_')
colnames(data)[2] <- 'filename'
data2 <- separate(data, col = filename, remove = FALSE,
                  into= c('plot', 'date', 'pre_post_dawn', 'sample_no'), sep = '_')
data2 <- data2[,c(1,2,4:6,9)]

#remove duplicate species per file, and species level data
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
colnames(data4)[1] <- 'plot'
#remove 7 plots that are not included in days 2-4
data4.5 <- data4[,c(1,3)]
data4.6 <- unique(data4.5)
table(data4.6$plot)

data5 <- data4[!data4$plot == 'CFB015' & !data4$plot == 'CFB028' & !data4$plot == 'CFB031' &
                 !data4$plot == 'CFB066' & !data4$plot == 'CFB101' & !data4$plot == 'CFB111' &
                 !data4$plot == 'CFB129' & !data4$plot == 'CFB133' & !data4$plot == 'CFB180',] 
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
      combo_x$Freq <- 1
      combo_x$combination <- paste('duration',durations[i],'_intensity', intensities[j], '_dayphase',dayphases[k])
      blank <- rbind(blank, combo_x)
    }
  }
}
data6 <- blank 
#data6 is the presence/absence matrix for each scenario 
data7 <- data6 %>% spread(Var1, Freq)
combos <- data7$combination
data7[is.na(data7)] <- 0
rownames(data7) <- data7[,1]
data7 <- data7[,-1]
data7 <- mutate_all(data7, function(x) as.numeric(as.character(x)))
#duration 17, 18, 19, 20 = days 1, 2, 3, 4
#intensity 9, 10, 11, 12, 13 = every 3, 6, 15, 30, 60
#dayphase 14, 15, 16 = dawn, morning, both

test <- beta.pair(data7)
turnover <- as.data.frame(as.matrix(test$beta.sim))
nestedness <- as.data.frame(as.matrix(test$beta.sne))
#write.csv(turnover, 'beta_turnover_plots_pooled.csv')
#write.csv(nestedness, 'beta_nestedness_plots_pooled.csv')

#values were then manually put into tables in order to create Figs. 5,6 and 9
