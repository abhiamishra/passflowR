install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
library(tidyverse)
library(ggplot2)
library(dplyr)

install.packages("devtools")
library(devtools)
devtools::install_github("statsbomb/StatsBombR")

library(StatsBombR)

install.packages("ggsoccer")
library(ggsoccer)

install.packages("viridis")
library(viridis)


CompAnalyze = FreeCompetitions() %>%
  filter(competition_id == 11, season_name == "2017/2018")

Matches = FreeMatches(CompAnalyze)

MatchEventData = StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)

MatchEventData = allclean(MatchEventData)

passdata = MatchEventData %>%
  filter(type.name == "Pass", team.name == "Barcelona")

passdata$player.name

p_name = "Lionel Andrés Messi Cuccittini"


bin <- 20

x_bin <- 120/bin
y_bin <- 80/bin

passfx <- seq(0,120,by=bin)
passfy = seq(0,80,by=bin)

PassFlow <- data.frame("x"=0.0,"y"=0.0,"endX"=0.0,"endY"=0.0, countP=0.0)

PlayerPF <- passdata %>% filter(player.name == p_name)

for(i in 1:x_bin){
  
  filterx <- PlayerPF %>% filter(location.x>=passfx[i]) %>%
    filter(location.x<passfx[i+1])
  
  for(j in 1:y_bin){
    
    minY = passfy[j]
    maxY = passfy[j+1]
    
    filtery <- filterx %>% filter(location.y>=minY) %>%
      filter(location.y<maxY)
    
    if(nrow(filtery)>=1){
      
      me_x = mean(filtery$location.x)
      me_y = mean(filtery$location.y)
      me_ex = mean(filtery$pass.end_location.x)
      me_ey = mean(filtery$pass.end_location.y)
      
      count = nrow(filtery)
      
      x <- c(me_x,me_y,me_ex,me_ey,count)
      PassFlow <- rbind(PassFlow, x)
      
    }
    
  }
  
}

PassFlow <- PassFlow[2:nrow(PassFlow), ]

write.csv(PassFlow, "D:\\R\\PassFlowTutorial\\PassFlow.csv", row.names = FALSE)

PassFlow %>%
  ggplot()+
  annotate_pitch(dimensions = pitch_statsbomb, colour = "white",
                 fill = "#141622")+
  theme_pitch()+
  geom_bin2d(data=PlayerPF,aes(x=location.x,y=location.y),alpha=0.6,
             binwidth = c(bin, bin), position = "identity")+
  scale_fill_viridis()+
  geom_segment(aes(x=x,y=y,xend=endX,yend=endY,alpha=countP),
               color="white",lineend = "round", size=2, arrow = arrow(length = unit(0.08, "inches")))+
  scale_y_reverse()
