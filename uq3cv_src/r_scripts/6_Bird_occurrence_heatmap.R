library(dplyr)
library(tidyverse)

setwd()
timing <- readxl::read_xlsx("identification_data_raw_rain_removed.xlsx")
pre <- timing[grep("predawn", timing$`Filename (one-minute sample)`), ]
pre$timeofday <- "pre"
pre$timerelationdawn <- substr(pre$`Filename (one-minute sample)`, 25, 26)
pre1 <- pre [, c(2, 4, 5, 10, 11)]
pre2 <- unique(pre1)
pre2$species <- paste(pre2$Genus, sep="_", pre2$Species)
pre3 <- pre2[ c(4, 5, 6)]
pre3$timerelationdawn <- as.numeric(pre3$timerelationdawn)
pre4 <- pre3
pre4$timerelationdawn[pre4$timerelationdawn == 1] <- -60
pre4$timerelationdawn[pre4$timerelationdawn == 2] <- -57
pre4$timerelationdawn[pre4$timerelationdawn == 3] <- -54
pre4$timerelationdawn[pre4$timerelationdawn == 4] <- -51
pre4$timerelationdawn[pre4$timerelationdawn == 5] <- -48
pre4$timerelationdawn[pre4$timerelationdawn == 6] <- -45
pre4$timerelationdawn[pre4$timerelationdawn == 7] <- -42
pre4$timerelationdawn[pre4$timerelationdawn == 8] <- -39
pre4$timerelationdawn[pre4$timerelationdawn == 9] <- -36
pre4$timerelationdawn[pre4$timerelationdawn == 10] <- -33
pre4$timerelationdawn[pre4$timerelationdawn == 11] <- -30
pre4$timerelationdawn[pre4$timerelationdawn == 12] <- -27
pre4$timerelationdawn[pre4$timerelationdawn == 13] <- -24
pre4$timerelationdawn[pre4$timerelationdawn == 14] <- -21
pre4$timerelationdawn[pre4$timerelationdawn == 15] <- -18
pre4$timerelationdawn[pre4$timerelationdawn == 16] <- -15
pre4$timerelationdawn[pre4$timerelationdawn == 17] <- -12
pre4$timerelationdawn[pre4$timerelationdawn == 18] <- -9
pre4$timerelationdawn[pre4$timerelationdawn == 19] <- -6
pre4$timerelationdawn[pre4$timerelationdawn == 20] <- -3

pre5 <- pre4
pre5$Freq <- ave(pre5$timerelationdawn, pre5[,c("timerelationdawn","species")], FUN=length)
pre6 <- pre5
pre6 <- unique(pre5)

timing <- readxl::read_xlsx("identification_data_cleaned.xlsx", sheet = 3)
post <- timing [grep("postdawn", timing$`Filename (one-minute sample)`), ]
post$timeofday <- "post"
post$timerelationdawn <- substr(post$`Filename (one-minute sample)`, 26, 27)
post1 <- post [, c(2, 4, 5, 10, 11)]
post2 <- unique(post1)
post2$species <- paste(post2$Genus, sep="_", post2$Species)
post3 <- post2[ c(4, 5, 6)]
post3$timerelationdawn <- as.numeric(post3$timerelationdawn)
post4 <- post3
post4$timerelationdawn[post4$timerelationdawn == 20] <- 60
post4$timerelationdawn[post4$timerelationdawn == 19] <- 57
post4$timerelationdawn[post4$timerelationdawn == 18] <- 54
post4$timerelationdawn[post4$timerelationdawn == 17] <- 51
post4$timerelationdawn[post4$timerelationdawn == 16] <- 48
post4$timerelationdawn[post4$timerelationdawn == 15] <- 45
post4$timerelationdawn[post4$timerelationdawn == 14] <- 42
post4$timerelationdawn[post4$timerelationdawn == 13] <- 39
post4$timerelationdawn[post4$timerelationdawn == 12] <- 36
post4$timerelationdawn[post4$timerelationdawn == 11] <- 33
post4$timerelationdawn[post4$timerelationdawn == 10] <- 30
post4$timerelationdawn[post4$timerelationdawn == 9] <- 27
post4$timerelationdawn[post4$timerelationdawn == 8] <- 24
post4$timerelationdawn[post4$timerelationdawn == 7] <- 21
post4$timerelationdawn[post4$timerelationdawn == 6] <- 18
post4$timerelationdawn[post4$timerelationdawn == 5] <- 15
post4$timerelationdawn[post4$timerelationdawn == 4] <- 12
post4$timerelationdawn[post4$timerelationdawn == 3] <- 9
post4$timerelationdawn[post4$timerelationdawn == 2] <- 6
post4$timerelationdawn[post4$timerelationdawn == 1] <- 3

post5 <- post4
post5$Freq <- ave(post5$timerelationdawn, post5[,c("timerelationdawn","species")], FUN=length)
post6 <- post5
post6 <- unique(post5)

#load pre- and post-dawn (later named dawn and morning) data into one table
q3_data <- rbind(pre6, post6)
q3_data <- q3_data[, c(2,3,4)]

fnct <- function(x) paste(unlist(strsplit(x,"[_]")),collapse=" ")
q3_data2 <- q3_data
q3_data2$species <- sapply(q3_data2$species,fnct)


library(ggplot2)
ggplot(q3_data2, aes(x=timerelationdawn, y=reorder(species, timerelationdawn))) + #reorder species
  geom_tile(aes(fill=Freq), colour="white", size=0.2) + 
  scale_fill_gradient(low="lightcyan2", high="chartreuse4") +
  geom_vline(xintercept = 0, linetype = 5) +                            #adds line at dawn
  ggplot2::annotate("text", 1.5, 2.25, label = "Dawn", angle = 90) +    #text that describes line
  annotate("rect", xmin = -1, xmax = 1, ymin = 0, ymax = 53.5, alpha =.25, fill="gray30") +
  theme_minimal() + theme(axis.ticks = element_line(size=0.3), 
                          axis.line = element_line(color = "gray60"),
                          legend.key.width = grid::unit(0.25,"cm"),
                          legend.title.align = 0) +
  scale_x_continuous(expand = c(0, 0),                                  
                     breaks = seq(-60, 60, 3), limits=c(-63, 63)) +    #changes x-axis intervals
  scale_y_discrete(expand = c(0, 0), limits=rev) +
      labs(fill= "No. Plots",                                     
           title="Bird Species Occurrence in Relation to Dawn",
      x = "Time in Relation to Dawn (min)",                             
      y = "Bird Species")
      

