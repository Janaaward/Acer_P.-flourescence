#4/14/25
#cleaning photo data combined
#load the package
library (Rcmdr, lessR, car, Hmisc)
library (vioplot)
library (lessR)
library(ggpubr)
library (dplyr)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(here)
library(readxl)
library(forcats)

#set seed
set.seed(22)
#set working directory
setwd("/Users/janaaward/Documents/GitHub/Acer_P.-flourescence")
#session - set working directory
library (ggplot2)
Combofluor <-Read()
Combofluor<-read_csv("Combo photo.csv", na = c(".", ".."))
Fluor<- Fluorescence_22_24_adjusted
# Keep a subset of rows using a logical expression
Fluor <- Fluorescence_22_24_adjusted[Fluorescence_22_24_adjusted$keep >= 1, ]
Fluor <- filter(Fluorescence_22_24_adjusted, keep=="1")
Fluorescence_22_24_adjusted <- readxl::read_xlsx("Fluorescence 22-24_adjusted.xlsx", "Sheet2", na = ".")

#########clean up data ##############
# pull out just ones to keep

#changing into factors and numeric
# e.g. 
Fluor$year <- as.factor(Fluor$year)
Fluor$field_season <- as.factor(Fluor$field_season)
Fluor$field_cond_score <- as.factor(Fluor$field_cond_score)
Fluor$tree_code <- as.factor(Fluor$tree_code)
Fluor$Sex_MFNB <- as.factor(Fluor$Sex_MFNB)
Fluor$mean_fvfm <- as.numeric(Fluor$mean_fvfm)
Fluor$site <- as.factor(Fluor$site)
Fluor$transect <- as.factor(Fluor$transect)
Fluor$keep <- as.factor(Fluor$keep)
Fluor$time <- as.numeric(Fluor$time)
Fluor$Time_01 <- as.numeric(Fluor$Time_01)
# Checking variables 
summary(Fluor$mean_fvfm)
summary(Fluor$field_season)
summary(Fluor$Sex_MFNB)
summary(Fluor$field_cond_score)
summary(Fluor$tree_code)
summary(Fluor$year)
summary(Fluor$site)
summary(Fluor$transect)
summary(Fluor$keep)
levels(Fluor$keep)
summary
# Check the structure
str(Fluor)

## spring
Fluor_Spring <- filter(Fluor, field_season=="spring")
Fluor_Spring$Sex_MFNB<- ordered(Fluor_Spring$Sex_MFNB, levels = c("N", "M", "B", "F"))
# graphing ggplot
condition.plot.Spring <- Fluor_Spring %>% group_by(Sex_MFNB) %>%
  ggplot(aes(x = Sex_MFNB, y = mean_fvfm, fill = Sex_MFNB)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_crossbar(data = Fluor_Spring, 
                aes(x = Sex_MFNB, y = mean_fvfm, ymin = mean_fvfm, ymax = mean_fvfm),
                color = "black",
                width = 0.75, 
                position = position_dodge(width = 0.5),
                linewidth = 0,
                linetype ="dashed") +
  #scale_fill_brewer(palette="Dark2", labels = c("Non-reproductive", "Male", "Monoecious", "Female")) +
  scale_fill_manual(values = c("N" = "#228B22", "M" = "#1870d5", "B" = "#FFF017", "F" = "#fe7ec0"), labels = c("Non-Reproductive", "Male", "Monoecious", "Female")) +
  theme_minimal() +
  scale_x_discrete(name = "Tree Sex", labels = c("Non-reproductive", "Male", "Monoecious", "Female")) + 
  ylab("Fluorescence Rate (Fv/m)") +
  ylim(0.6,0.9) +
  # theme(axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) +
  #guides(fill = guide_legend(title=element_blank(), position = "top")) +
  ggtitle("Spring") +
  theme(plot.title = element_text(size = 20), legend.position = "none")

#theme(legend.position="top")
# guides(fill = guide_legend(title="Tree tree_sex", position = "bottom"))

condition.plot.Spring


#########
### graph for summer
Fluor_Summer <- filter(Fluor, field_season=="summer")
Fluor_Summer$Sex_MFNB<- ordered(Fluor_Summer$Sex_MFNB, levels = c("N", "M", "B", "F"))
# graphing ggplot
ggboxplot (Fluor_Summer, x = "Sex_MFNB", y = "mean_fvfm", 
           color = "Sex_MFNB", palette = c( "N" ="#228B22","M" = "#1870d5","B" = "#FFF017" ,"F" = "#fe7ec0"),
           ylab = "flourescence rate", xlab = "sex")


#### modifying sam's condition plot to fit fluor
condition.plot.Summer <- Fluor_Summer %>% group_by(Sex_MFNB) %>%
  ggplot(aes(x = Sex_MFNB, y = mean_fvfm, fill = Sex_MFNB)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_crossbar(data = Fluor_Summer, 
                aes(x = Sex_MFNB, y = mean_fvfm, ymin = mean_fvfm, ymax = mean_fvfm),
                color = "black",
                width = 0.75, 
                position = position_dodge(width = 0.5),
                linewidth = 0,
                linetype ="dashed") +
  #scale_fill_brewer(palette="Dark2", labels = c("Non-reproductive", "Male", "Monoecious", "Female")) +
  scale_fill_manual(values = c("N" = "#228B22", "M" = "#1870d5", "B" = "#FFF017", "F" = "#fe7ec0"), labels = c("Non-Reproductive", "Male", "Monoecious", "Female")) +
  theme_minimal() +
  scale_x_discrete(name = "Tree Sex", labels = c("Non-reproductive", "Male", "Monoecious", "Female")) + 
  ylab("Fluorescence Rate (Fv/m)") +
  ylim(0.6,0.9) +
  # theme(axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) +
  #guides(fill = guide_legend(title=element_blank(), position = "top")) +
  ggtitle("Summer") +
  theme(plot.title = element_text(size = 20))
#theme(legend.position="top")
# guides(fill = guide_legend(title="Tree tree_sex", position = "bottom"))
condition.plot.Summer
#################
###jitter plot
##spring jitter plot
##########
# adding jitter 
condition.plot.Spring <- Fluor_Spring %>% group_by(Sex_MFNB) %>%
  ggplot(aes(x = Sex_MFNB, y = mean_fvfm, fill = Sex_MFNB)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.45, dodge.width = 0.8), alpha = 0.13) +
  geom_crossbar(data = Fluor_Spring, 
                aes(x = Sex_MFNB, y = mean_fvfm, ymin = mean_fvfm, ymax = mean_fvfm),
                color = "black",
                width = 0.75, 
                position = position_dodge(width = 0.8),
                linewidth = 0,
                linetype ="dashed") +
  #scale_fill_brewer(palette="Dark2", labels = c("Non-reproductive", "Male", "Monoecious", "Female")) +
  scale_fill_manual(values = c("N" = "#228B22", "M" = "#1870d5", "B" = "#FFF017", "F" = "#fe7ec0"), labels = c("Non-Reproductive", "Male", "Monoecious", "Female")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),  
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))+
  scale_x_discrete(name = "Tree Sex", labels = c("Non-reproductive", "Male", "Monoecious", "Female")) + 
  ylab("Fluorescence Rate (Fv/m)") +
  ylim(0.6,0.9) +
  # theme(axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title=element_blank(), position = "top")) +
  ggtitle("Spring") +
  theme(plot.title = element_text(size = 20), legend.position = "none")

#theme(legend.position="top")
# guides(fill = guide_legend(title="Tree tree_sex", position = "bottom"))

condition.plot.Spring
#summer jitter plot
##########
# adding jitter 
condition.plot.Summer <- Fluor_Summer %>% group_by(Sex_MFNB) %>%
  ggplot(aes(x = Sex_MFNB, y = mean_fvfm, fill = Sex_MFNB)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.45, dodge.width = 0.8), alpha = 0.13) +
  geom_crossbar(data = Fluor_Summer, 
                aes(x = Sex_MFNB, y = mean_fvfm, ymin = mean_fvfm, ymax = mean_fvfm),
                color = "black",
                width = 0.725, 
                position = position_dodge(width = 0.8),
                linewidth = 0,
                linetype ="dashed") +
  #scale_fill_brewer(palette="Dark2", labels = c("Non-reproductive", "Male", "Monoecious", "Female")) +
  scale_fill_manual(values = c("N" = "#228B22", "M" = "#1870d5", "B" = "#FFF017", "F" = "#fe7ec0"), labels = c("Non-Reproductive", "Male", "Monoecious", "Female")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),  
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))+
  scale_x_discrete(name = "Tree Sex", labels = c("Non-reproductive", "Male", "Monoecious", "Female")) + 
  ylab("Fluorescence Rate (Fv/m)") +
  ylim(0.6,0.9) +
  # theme(axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title=element_blank(), position = "top")) +
  ggtitle("Summer") +
  theme(plot.title = element_text(size = 20), legend.position = "none")

#theme(legend.position="top")
# guides(fill = guide_legend(title="Tree tree_sex", position = "bottom"))

condition.plot.Summer

#save as a csv
write.csv(Combophoto,"Cleanedphoto", row.names=FALSE)