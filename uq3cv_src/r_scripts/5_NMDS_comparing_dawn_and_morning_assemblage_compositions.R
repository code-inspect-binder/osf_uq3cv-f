rm(list = ls())
setwd()

library(dplyr)
library(reshape2)
library(gtools)
library(vegan)
library(ggplot2)
library(tidyverse)
#need species matrices for each combination, plot as row, species as column.

data <- readxl::read_xlsx('identification_data_cleaned.xlsx')
data <- data[,c(2,3,4,5)]
data$species <- paste(data$Genus,data$Species, sep = '_')
colnames(data)[2] <- 'filename'
data2 <- separate(data, col = filename, remove = FALSE,
                  into= c('plot', 'date', 'pre_post_dawn', 'sample_no'), sep = '_')
data2.5 <- data2[!data2$species == 'Certhia_sp.',]
data2.6 <- data2.5[!data2.5$species == 'Regulus_sp.',]
data3 <- data2.6 %>% group_by(Plot_no) %>% mutate(whichday = as.integer(factor(date)))
data4 <- data3 %>% group_by(Plot_no, filename) %>% mutate(seq=cur_group_id())

###########create a new community matrix for each combination##############
data4$intensity1 <- 1
data4$intensity2 <- ifelse(data4$sample_no == 1 |  data4$sample_no == 3 |data4$sample_no == 5 |data4$sample_no == 7 |
                             data4$sample_no == 9 |data4$sample_no == 11 | data4$sample_no == 13 |data4$sample_no == 15 |
                             data4$sample_no == 17 |data4$sample_no == 19, 1, 0)
data4$intensity3 <- ifelse(data4$sample_no == 1 |  data4$sample_no == 6 |data4$sample_no == 11 |data4$sample_no == 16, 1, 0)
data4$intensity4 <- ifelse(data4$sample_no == 1 |  data4$sample_no == 11, 1, 0)
data4$intensity5 <- ifelse(data4$sample_no == 1, 1, 0)
data4$dayphase_pre <- ifelse(data4$pre_post_dawn == 'predawn', 1, 0)
data4$dayphase_post <- ifelse(data4$pre_post_dawn == 'postdawn', 1, 0)
data4$dayphase_both <- 1
data4$duration1 <- ifelse(data4$whichday < 2, 1, 0)
data4$duration2 <- ifelse(data4$whichday < 3, 1, 0)
data4$duration3 <- ifelse(data4$whichday < 4, 1, 0)
data4$duration4 <- ifelse(data4$whichday < 5, 1, 0)

#remove 7 plots that are not included in days 2-4
data4.5 <- data4[,c(3,4)]
data4.6 <- unique(data4.5)
table(data4.6$plot)

data5 <- data4[!data4$plot == 'CFB015' & !data4$plot == 'CFB028' & !data4$plot == 'CFB031' &
                 !data4$plot == 'CFB066' & !data4$plot == 'CFB101' & !data4$plot == 'CFB111' &
                 !data4$plot == 'CFB129' & !data4$plot == 'CFB133' & !data4$plot == 'CFB180',] 
data4 <- data5

#sca_predawn = 1 day, every six minutes, predawn only (n=20)
sca_predawn <- data4[data4$dayphase_pre == 1 & data4$intensity3 == 1 & data4$duration2 == 1,]
#sca_postdawn = 1 day, every six minutes, postdawn only (n=20)
sca_postdawn <- data4[data4$dayphase_post == 1 & data4$intensity3 == 1 & data4$duration2 == 1,]
#sca_both = 1 day, every six minutes, both predawn and postdawn (n=40)
sca_both <- data4[data4$intensity3 == 1 & data4$duration2 == 1,]

#make community matrices for each
sca_predawn <- sca_predawn[,c(1,9)]
sca_postdawn <- sca_postdawn[,c(1,9)]
sca_both <- sca_both[,c(1,9)]

sca_predawn_wide <- dcast(sca_predawn, Plot_no ~ species, fun.aggregate = length)
sca_postdawn_wide <- dcast(sca_postdawn, Plot_no ~ species, fun.aggregate = length)
sca_both_wide <- dcast(sca_both, Plot_no ~ species, fun.aggregate = length)

#reshape dataframes
rownames(sca_predawn_wide) <- sca_predawn_wide$Plot_no
colnames(sca_predawn_wide)[1] <- "plot_id"
rownames(sca_postdawn_wide) <- sca_postdawn_wide$Plot_no
colnames(sca_postdawn_wide)[1] <- "plot_id"
rownames(sca_both_wide) <- sca_both_wide$Plot_no
colnames(sca_both_wide)[1] <- "plot_id"

#add combination to plot
sca_predawn_wide$plot_id <- paste(sca_predawn_wide$plot_id, '_predawn', sep = '')
sca_postdawn_wide$plot_id <- paste(sca_postdawn_wide$plot_id, '_postdawn', sep = '')
sca_both_wide$plot_id <- paste(sca_both_wide$plot_id, '_both', sep = '')

#merge dataframes 
species_matrix <- merge(sca_predawn_wide, sca_postdawn_wide, all = TRUE)
#species_matrix <- merge(species_matrix, sca_both_wide, all = TRUE)
species_matrix[is.na(species_matrix)] <- 0
rownames(species_matrix) <- species_matrix$plot_id
species_matrix <- species_matrix[,-1]

species_matrix_short <- species_matrix

species_matrix_short.nmds <- metaMDS(comm = species_matrix_short, distance ="euclidean", k=2, maxit = 999,
                                                         trace = FALSE, trymax = 250)
species_matrix_short.nmds

#to see how well the distances are measured
stressplot(species_matrix_short.nmds) #R2 = 0.961, linear R2 = 0.891

data.scores <- as.data.frame(scores(species_matrix_short.nmds$points))
data.scores$site <- substr(rownames(data.scores),1,6)
data.scores$plots <- substr(rownames(data.scores),8,15)

species.scores <- as.data.frame(scores(species_matrix_short.nmds, "species"))
species.scores$species <- rownames(species.scores)

#create polygon with the outermost plots
plots_pre <- data.scores[data.scores$plots == "predawn", ][chull(data.scores[data.scores$plots
                                                                             == "predawn", c("MDS1", "MDS2")]), ]
plots_post <- data.scores[data.scores$plots == "postdawn", ][chull(data.scores[data.scores$plots
                                                                               =="postdawn", c("MDS1", "MDS2")]), ]
polygon.data <- rbind(plots_pre, plots_post)


text_species <- species.scores

#now statistical test to see if communities are statistically different from one another
species_matrix2 <- species_matrix %>% mutate_if(is.numeric, ~1 * (. > 0))
sp_matrix_with_group <- species_matrix2
rownames(sp_matrix_with_group) <- rownames(species_matrix)
sp_matrix_with_group$grouping <- substr(rownames(sp_matrix_with_group), 7,13)
ano <- anosim(species_matrix2, distance = 'euclidean', grouping = sp_matrix_with_group$grouping)
summary(ano)

#plot nmds
library(ggrepel)
ggplot() + 
  geom_text_repel(data=text_species, aes(x=NMDS1, y=NMDS2, label=species), alpha=0.7) + #species labels
  geom_point(data=data.scores, aes(x=MDS1, y=MDS2, colour=plots), size=2) +  #points for plots
  # geom_point(data=cross_species, aes(x=NMDS1, y=NMDS2), shape=4, size=2, stroke=1.3, color="gray35") + #species crosses
 geom_polygon(data=polygon.data, aes(x=MDS1, y=MDS2, fill=plots), alpha=0.3) + #polygon shape
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(.95, .05),
        legend.justification = c("right", "bottom"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0)) +
  scale_colour_manual(values=c("predawn" = "mediumpurple1", "postdawn" = "mediumpurple4")) +
  scale_fill_manual(values=c("predawn" = "mediumpurple1", "postdawn" = "mediumpurple4")) +
 # scale_x_continuous(limits = c(-32, 25), breaks = NULL) +
#  scale_y_continuous(limits = c(-25, 24), breaks = NULL) +
  labs(colour= "Plots", fill="Plots") + 
  ggtitle("Predawn x Postdawn", subtitle = "Intensity = Every 15 Mins; Duration = 2 Days") +
  labs(caption="Stress Value = 0.208; ANOSIM R = 0.166**")


