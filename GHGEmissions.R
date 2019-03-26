setwd("/Users/mkumari/Downloads")
library(ggplot2)
library(gridExtra)
library(reshape2)
library(scales)
library(zoom)
library(cowplot)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(forcats)
# Generalized Cost
df <- read.csv("MK_FromMarshall - GHGEmission-3.csv", header = TRUE)

df_1clean <- select(df, -Change.in.Emission,  -Year, -Scenario)
df_1Melted <- melt(df_1clean, id.var = c("Melt", "Color"))
colourCount = length(unique(df_1clean$Melt))
df_1clean %>% 
  dplyr::mutate(Melt = factor(Melt, 
                                    levels = c("LDVsBAU2015",
                                               "LDVsBAU2030",
                                               "LDVsBAU2050",
                                               "LDVsZEV2030",
                                               "LDVsZEV2050",
                                               "TrucksBAU2015",
                                               "TrucksBAU2030",
                                               "TrucksBAU2050",
                                               "TrucksZEV2030",
                                               "TrucksZEV2050",
                                               "LDVs+TrucksBAU2015",
                                               "LDVs+TrucksBAU2030",
                                               "LDVs+TrucksBAU2050",
                                               "LDVs+TrucksZEV2030",
                                               "LDVs+TrucksZEV2050"))) %>% 
ggplot( aes(x = Melt ,y = df_1clean$X.ktCO2e. / 1000, fill=Color)) + 
  #ylim(0,800)+
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(colourCount))+
  annotate(geom = "text", x = seq_len(nrow(df)) ,
           y = -10, label = c(2015,2030,2050,2030,2050,2015,2030,2050,2030,2050,2015,2030,2050,2030,2050),
           size = 5, angle=90, hjust=1) +
  annotate(geom = "text", x = c(2,4.5,7,9.5,12,14.5), y = -75, label = c("BAU", "ZEV","BAU", "ZEV","BAU", "ZEV"), size = 5) +
  annotate(geom = "text", x = c(3,8,13), y = -105, label = c("LDVs", "Trucks", "Combined"), size = 5) +
  annotate("segment", x = 0.5, xend = 0.5, y = -115, yend = 225,size=1)+
  annotate("segment", x = 3.5, xend = 3.5, y = -90, yend = 0)+
  annotate("segment", x = 5.5, xend = 5.5, y = -115, yend = 225,size=1)+
  annotate("segment", x =8.5, xend = 8.5, y = -90, yend = 0)+
  annotate("segment", x = 10.5, xend = 10.5, y = -115, yend = 225,size=1)+
  annotate("segment", x =13.5, xend = 13.5, y = -90, yend = 0)+
  annotate("segment", x = 15.5, xend = 15.5, y = -115, yend = 225,size=1)+
  annotate("segment", x = 0.5, xend = 15.5, y = -115, yend = -115)+
  annotate("segment", x = 0.5, xend = 15.5, y = -90, yend = -90)+
  
  annotate("segment", x = 0.6, xend = 5.5, y = df_1clean$X.ktCO2e.[1]/1000, yend = df_1clean$X.ktCO2e.[1]/1000, 
           linetype = 2, color="grey50")+
  annotate("segment", x = 5.6, xend = 10.5, y = df_1clean$X.ktCO2e.[6]/1000, yend = df_1clean$X.ktCO2e.[6]/1000,
           linetype = 2, color="grey50")+
  annotate("segment", x = 10.6, xend = 15.5, y = df_1clean$X.ktCO2e.[11]/1000, yend = df_1clean$X.ktCO2e.[11]/1000,
           linetype = 2, color="grey50")+
  
  annotate(geom = "text", x = seq_len(nrow(df))-0.1, y = (df_1clean$X.ktCO2e./1000) + 15, 
           label = as.integer(df$Change.in.Emission*100), size = 5) +
  annotate(geom = "text", x = seq_len(nrow(df))+0.3,y = (df_1clean$X.ktCO2e./1000) + 15, 
           label = rep(c("","%","%","%","%"), times=3) , size = 5) +
  labs(title = "GHG Emissions", y="Emissions (mt CO2e)") + 
  coord_cartesian(ylim = c(0, 225), expand = FALSE, clip = "off") +
  theme_bw()+ 
  theme( plot.margin = unit(c(1, 1, 12, 1), "lines"),plot.title = element_text(size=20,hjust=0.5),
         axis.title.x = element_blank(),axis.title.y = element_text(size=15),axis.text.y = element_text(size=15),
         axis.text.x = element_blank(),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(), legend.position = "none")



